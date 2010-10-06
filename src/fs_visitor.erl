%% This works!
% File Finder
% but we need to integrate the path names....
% Tag == BinDir == directory to visit right now.
% 
% See stub for simple send...
% Want to pass in the action....
%
%
% Todo - Save UID/Cookie
% Be given a function to apply to file.
%
-module(fs_visitor).
-author("raballa@sandia.gov").
-vsn("1.0").

-export([visit_files/2, do_work/1]).

-define(MAXLEN, 10).
-define(SLEEPSEC, 2).

% Longer checks to see if a list is longer than N elements
% I need it since list:split throws an exception if the list being split is
% not long enough.
%
% Replaced 9/21 since I can now catch the exception in visit_files.

%% longer(N, List) ->  longer(N, List, 0).
%% longer(N, _List, N) ->  true;
%% longer(N, [], M) ->  false;
%% longer(N, [ _H | T], M) -> longer(N, T, M+1).

%% simple_send(Cmd, Data) ->
%%     server ! {self(), {Cmd, Data}},
%%     receive
%%         {server, ok} ->
%%              ok;
%%         _ ->
%%             % Error logger
%%             io:format("Huh - simple send did not get an OK")
%%     end.
simple_send(Cmd, Data) ->
    io:format("Send ~p to server~n", [{Cmd, Data}]).

loop(X) ->
    fs_server:get_work(),
    receive
        {server, reply {visit, Pkg}} ->
            fs_event:info_message("~w Got work package~n", self(), Pkg),
            Visited = catch(do_work(Pkg)),
            % check for ok/notok?
            fs_server:work_complete(Pkg, Visited),
            loop(X);

        {server, reply {no_work} } ->
            %log sleep
            fs_event:info_msg("~w sleeping~n", self()),
            timer:sleep(?SLEEPSEC * 1000),
            loop(X);

        % log stop?
        { Pid, stop } -> ok;

        % Handle unexpected messages?
        Msg  -> fs_event:error_msg("Unexpected message ~p~n?", [Msg]),
                loop(X)
    end.

% This code an the next clause are really similar. Can you refactor
% to reduce redundancy
do_work({directory, BinDir, RootDir}) ->
    % Check if file exists, readable, executable...
    {ok, Dir} = ok_to_visit_directory(BinDir, RootDir),
    {ok, Cwd} = file:get_cwd(),
    file:set_cwd(Dir),
    {ok, Files} = file:wildcard("*", Dir),
    visit_files(Files, list_to_binary(Dir)),
    file:set_cwd(Cwd),
    ok.

do_work({files, FileList, BinDir}) ->
    {ok, Dir} = ok_to_visit_directory(BinDir),
    {ok, Cwd} = file:get_cwd(),
    file:set_cwd(Dir),
    visit_files(lists:map(binary_to_list(FileList)), list_to_binary(Dir)),
    file:set_cwd(Cwd),
    ok.

ok_to_visit_directory(BinDir) ->
    Dir = binary_to_list(BinDir),
    % exists
    % is directory
    % readable
    % executable
    {ok, Dir}.

add_work({dir, Dir}, BinDir) ->
    fs_server:add_work({directory, list_to_binary(Dir), BinDir});

add_work({files, FileList}, BinDir) ->
    add_filtered_files(FileList, [], BinDir).

%lists:map(fun(X)->list_to_binary(X) end, FileList) }).                    
add_filtered_files([], Files, BinDir) ->
    fs_server:add_work({files, Files, BinDir})

add_filtered_files([ H | T ], Files, BinDir) ->
    case filelib:is_dir(H) of 
        true -> io:format("Adding Directory ~p~n", [H]),
                add_work({dir, H}, BinDir),
                add_filtered_files(T, Files, BinDir);
        false -> add_filtered_files(T, [ list_to_binary(H) | Files ], BinDir)
    end.

%%%
visit_files(FileList, Tag) ->
    case catch(lists:split(?MAXLEN, FileList)) of
         {List1, List2} -> 
            add_work({files, List1}, Tag),
            visit_files(List2, Tag);
        {'EXIT', Why } ->
            visit_list(FileList, Tag, 0)
    end.

visit_list([], Tag, N) ->
    fs_event:visit_update(N);

visit_list([H | T], Tag, N) -> 
    case filelib:is_dir(H) of 
        true -> add_work({dir, H}, Tag),
                visit_list(T, Tag, N);
        false -> do_file_visit(H),
                 visit_list(T, Tag, N+!)
    end.

do_file_visit(Filename) ->
    fs_event:info_message("Visiting ~p~n", [Filename]),
    ok.
