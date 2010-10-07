%%% This works!

%%% but we need to integrate the path names....
%%% Tag == BinDir == directory to visit right now.
%%% 
%%% Be given a filter/function to apply to file.
%%%

-module(fs_visitor).
-author("raballa@sandia.gov").
-vsn("1.0").

-include_lib("kernel/include/file.hrl").

-export([start/1, init/1, loop/1]).

-export([visit_files/2, do_work/1]).

-define(MAXLEN, 10).
-define(SLEEPSEC, 2).

start(Args) ->
    spawn(?MODULE, init, [Args]).

init(Args) ->
    loop(Args).
    
%%% Need to make this into normal processes.
loop(X) ->
    case fs_server:get_work() of
        {visit, Pkg} ->
            fs_event:info_message("~w Got work package ~p ~n", [self(), Pkg]),
            do_work(Pkg),
            fs_event:info_message("~w Completed work package ~p ~n", [self(), Pkg]),            fs_server:work_complete(Pkg),
            loop(X);

        {no_work}  ->
            %% log sleep
%%            fs_event:info_message("~w sleeping~n", [self()]),
            timer:sleep(?SLEEPSEC * 1000),
            loop(X);

        %% log stop?
        { _Pid, stop } ->
            io:format("Error in client"),
            ok;
        
        %% Handle unexpected messages?
        Msg  ->
            io:format("Unexpected message~p", [Msg]),
            fs_event:error_message("Unexpected message ~p~n?", [Msg]),
                loop(X)
    end.

%%% How do we concatenate strings?
make_path(BinDir, RootDir) ->
    Dir = binary_to_list(BinDir),
    Root = binary_to_list(RootDir),
    string:concat(Root, string:concat("/", Dir)).

do_work({directory, BinDir, RootDir}) ->
    %% Check if file exists, readable, executable...
    Dir = make_path(BinDir, RootDir),
    case ok_to_visit_directory(Dir) of
        {ok} ->  {ok, Cwd} = file:get_cwd(),
                 file:set_cwd(Dir),
                 Files = filelib:wildcard("*", Dir),
                 visit_files(Files, list_to_binary(Dir)),
                 file:set_cwd(Cwd);
        {error, Why } -> fs_event:info_message("Unable to visit directory ~p (~p) ~n", [Dir, Why])
    end,
    ok;

do_work({files, FileList, BinDir}) ->
    Dir = binary_to_list(BinDir),
    case ok_to_visit_directory(Dir) of
        {ok } ->
            {ok, Cwd} = file:get_cwd(),
            file:set_cwd(Dir),
            visit_files(lists:map(fun binary_to_list/1, FileList), BinDir),
            file:set_cwd(Cwd);
        {error, Why } ->
            fs_event:info_message("Unable to visit directory ~p (~p) ~n", [Dir, Why])
    end,
    ok.

%% 
ok_to_visit_directory(Dir) ->
    %% exists
    %% is directory
    %% readable
    %% executable
    {ok}.

add_work(dir, Dir, BinDir) ->
    fs_server:add_work({directory, list_to_binary(Dir), BinDir});

add_work(files, FileList, BinDir) ->
    add_filtered_files(FileList, [], BinDir).

add_filtered_files([], Files, BinDir) ->
    fs_server:add_work({files, Files, BinDir});

add_filtered_files([ H | T ], Files, BinDir) ->
    case filelib:is_dir(H) of 
        true -> %io:format("Adding Directory ~p~n", [H]),
            add_work(dir, H, BinDir),
            add_filtered_files(T, Files, BinDir);
        false -> add_filtered_files(T, [ list_to_binary(H) | Files ], BinDir)
    end.

%%%
visit_files(FileList, Tag) ->
    case catch(lists:split(?MAXLEN, FileList)) of
        {'EXIT', _Why } ->
            visit_list(FileList, Tag, 0);
        
        {List1, []} -> 
            visit_list(List1, Tag, 0);
        
        {List1, List2} -> 
            add_work(files, List1, Tag),
            visit_files(List2, Tag)
        end.

visit_list([], _Tag, N) ->
    fs_event:visit_update(N);

visit_list([H | T], Tag, N) -> 
    case filelib:is_dir(H) of 
        true ->  add_work(dir, H,  Tag), visit_list(T, Tag, N);
        false -> visit_file(H, Tag),    visit_list(T, Tag, N+1)
    end.

visit_file(Filename, Dir) ->
%%    fs_event:info_message("Visiting ~p~n", [Filename]),
    case file:read_file_info(Filename) of
        {ok, FileData} ->
            visit_file(Filename, Dir, FileData#file_info.type, FileData);

        {error, _} ->
            io:format("Unable to stat ~s ~n", [Filename])
    end,
    ok.

visit_file(Filename, Dir, regular, FileData)  ->
    Atime = FileData#file_info.atime,
    Mtime = FileData#file_info.mtime,
    Ctime = FileData#file_info.ctime,
    io:format("~s/~s ~b ~b ~b ~n", [Dir, Filename, as_epoch(Atime), as_epoch(Ctime), as_epoch(Mtime)]);

visit_file(Filename, Dir, Other, _FileData) ->
    fs_event:info_message("Ignoring ~w file  ~s to worklist!~n", [Other, Filename]).

% How can we factor out the constant?
as_epoch(Date) ->
    UnixEpoch = calendar:datetime_to_gregorian_seconds({{1970,1,1}, {0,0,0}}),
    calendar:datetime_to_gregorian_seconds(Date) - UnixEpoch.


