%%% This works!

%%% but we need to integrate the path names....
%%% Tag == BinDir == directory to visit right now.
%%% 
%%% Be given a filter/function to apply to file.
%%%
-module(fsw_worker).
-author("raballa@sandia.gov").
-vsn("0.2.0").

-export([start/1, init/3, loop/1]).

-record(state, {callback_module, callback_data}).

-define(MAX_FILES_PER_PKG, 10).
-define(SLEEPMSEC, 500).

% Args is a lsit [CallbackModule, Index]
start(Args) ->
    spawn(?MODULE, init, Args).

init(CallbackModule, Index, MaxClients) ->
    case apply({CallbackModule, init}, [Index, MaxClients]) of
        {ok, State } ->
            try
                fsw_eventlog:info_msg("worker starts looping~n", []),
                loop(#state{callback_module=CallbackModule, callback_data=State})
            after
                fsw_eventlog:info_msg("worker done looping~n", []),
                apply({CallbackModule, finalize}, [State])
            end;
        {error, Reason } ->
            io:format("Unable to open file: ~p~n", [Reason])
    end.
    
%%% Need to make this into normal processes.
loop(State) ->
    case fsw_blackboard:get_work() of
        {visit, Pkg} ->
            fsw_eventlog:info_msg("~w Got work package ~p ~n", [self(), Pkg]),
            perform_work_package(State, Pkg),
            fsw_eventlog:info_msg("~w Completed work package ~p ~n", [self(), Pkg]),
            fsw_blackboard:work_complete(Pkg),
            loop(State);

        {no_work}  ->
            %% log sleep
            fsw_eventlog:info_msg("~w sleeping~n", [self()]),
            timer:sleep(?SLEEPMSEC),
            loop(State);

        {done}  ->
            ok;            
        
        %% log stop?
%        { _Pid, stop } ->
%            io:format("Error in client"),
%            ok;
        
        %% Handle unexpected messages?
        Msg  ->
            fsw_eventlog:error_message("Unexpected message ~p~n?", [Msg]),
            loop(State)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Callback to module
visit_file(State, Filename) ->
    apply({State#state.callback_module, visit_file},
          [State#state.callback_data, Filename]).

%%% Callback to module
visit_directory(State,  Directory, When) ->
    apply({State#state.callback_module, visit_directory},
          [State#state.callback_data, Directory, When]).

%%% Callback to module
ok_to_visit_directory(State, Directory) ->
    apply({State#state.callback_module, ok_to_visit},
          [State#state.callback_data, Directory, directory]).
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Catenate the two strings as a full path name...
make_path(RootDir, BinDir) ->
    Dir = binary_to_list(BinDir),
    Root = binary_to_list(RootDir),
    filename:join(Root, Dir).


%%% perform_work_package/2 has 2 clauses; one for a directory package,
%%% and one for a file package
%%% They're really quite similar, but I've note found an elegant way to merge the 
%%% two yet!
perform_work_package(State, {directory, BinDir, RootDir}) ->
    %% make_path returns a list (not a binary).
    Dir = make_path(RootDir, BinDir),
    case ok_to_visit_directory(State, Dir) of
        {ok} ->
            visit_directory(State, Dir, pre),
            Files = filelib:wildcard("*", Dir),
            visit_files(State, Files, Dir, list_to_binary(Dir)),
            visit_directory(State, Dir, post);
        {skip} ->
            fsw_eventlog:info_message("Skippping visit to directory ~p (~p) ~n", [Dir]);
        
        {error, Why } ->
            fsw_eventlog:info_message("Unable to visit directory ~p (~p) ~n", [Dir, Why])
    end,
    ok;

%%% do the work for a file work package
perform_work_package(State, {files, FileList, BinDir}) ->
    Dir = binary_to_list(BinDir),
    %% This may be a redundant case, but there's no guarantee that the
    %% directory has not been deleted while this work package was in the
    %% in the todo list
    case ok_to_visit_directory(State, Dir) of
        {ok } ->
            Files = lists:map(fun binary_to_list/1, FileList),
            visit_files(State, Files, Dir,  BinDir);
        {skip} ->
            fsw_eventlog:info_message("Skippping visit to directory ~p (~p) ~n", [Dir]);
        
        {error, Why } ->
            fsw_eventlog:info_message("Unable to visit directory ~p (~p) ~n", [Dir, Why])
    end,
    ok.
%% 

%%% Helper functions to add work packages
add_directory_work_pkg(Dir, BinDir) ->
    fsw_blackboard:add_work({directory, list_to_binary(Dir), BinDir}).

add_file_work_pkg(FileList, BinDir) ->
    add_filtered_files(FileList, [], BinDir).

%%% add_filtered_files traverses a list of files,
%%%    if it finds a directory, it adds a directory work package to the server
%%%    otherwise, it converts the filename to a binary and cons's it onto the
%%%    result list, which is an accumulator.
%%% Once all the files are checked, it adds the file work list to the server
add_filtered_files([], [], _BinDir) ->
    ok; %ignore - no reason to add an empty file work package

add_filtered_files([], ResultFiles, BinDir) ->
    fsw_blackboard:add_work({files, ResultFiles, BinDir});

add_filtered_files([ H | T ], ResultFiles, BinDir) ->
    case filelib:is_dir(H) of 
        true ->
            add_directory_work_pkg(H, BinDir),
            add_filtered_files(T, ResultFiles, BinDir);
        false -> add_filtered_files(T, [ list_to_binary(H) | ResultFiles ], BinDir)
    end.


%%% visit_files/3 traverses a list of files and splits it into blocks of size MAX_FILES_PER_PKG.
%%% It also processes the files in the final block
visit_files(State, FileList, Dir, BinDir) ->
    case catch(lists:split(?MAX_FILES_PER_PKG, FileList)) of
        %% Calling lists:split on a list  shorter than MAX_FILES_PER_PKG throws an exception
        %% In this case, just process the files
        {'EXIT', _Why } ->
            process_file_list(State, FileList, Dir, 0);
        
        %% Exactly MAX_FILES_PER_PKG files in the list; process it
        {List1, []} -> 
            process_file_list(State, List1, Dir, 0);
        
        %% More than MAX_FILES_PER_PKG in the list. Put the first block into the worklist
        %% and recurse on the rest.
        {List1, List2} -> 
            add_file_work_pkg(List1, BinDir),
            visit_files(State, List2, Dir, BinDir)
        end.


%%% Function to call the visitor_callbacks on the files in the list.
%%% We could almost make this a mapping function, but the argument N 
%%% counts the number of actual files visited.
%%%
%%% Note that if we find a directory (which we shouldn't, I don't think!), but
%%% one can never be sure. The file system is not quiescent!
%%% then we add it to the work list and go on.
%%%
process_file_list(_State, [], _Dir, N) ->
    fsw_eventlog:visit_update(N);

process_file_list(State, [H | T], Dir, N) ->
    FullPath = filename:join(Dir, H),
    case filelib:is_dir(FullPath) of 
        %% I think the next case can't happen...
        true ->  add_directory_work_pkg(H,  list_to_binary(Dir)),
                 process_file_list(State, T, Dir, N);

        false -> visit_file(State, FullPath),
                 process_file_list(State, T, Dir, N+1)
    end.
