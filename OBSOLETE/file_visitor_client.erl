%% This works!
% but we need to integrate the path names....
% Tag == BinDir == directory to visit right now.
% 
% See stub for simple send...
%
-module(file_visitor_client).
-author("raballa@sandia.gov").
-vsn("1.0").

-export([visit_files/2, do_work/1]).

-define(MAXLEN, 10).
-define(SLEEPSEC, 2).

% Longer checks to see if a list is longer than N elements
% I need it since list:split throws an exception if the list being split is
% not long enough.
longer(N, List) ->  longer(N, List, 0).
longer(N, _List, N) ->  true;
longer(N, [], M) ->  false;
longer(N, [ _H | T], M) -> longer(N, T, M+1).

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
    server ! {self(), get_work},
    receive
        {server, package, Pkg} ->
            do_work(Pkg),
            loop(X);
        {server, no_work } ->
            timer:sleep(?SLEEPSEC * 1000),
            loop(X)
    end.


% What about errors!
% Try...
%
% Also, don't really want to walk the list twice...
% but we have to in this case...
% A file list is always short enough to just walk
% 
% What if directory does not exist...
do_work({dir, BinDir}) ->
    Dir = binary_to_list(BinDir),
    {ok, Cwd} = file:get_cwd(),
    file:set_cwd(Dir),
    {ok, Files} = file:wildcard(Dir ++ "/" ++ "*"),
    visit_files(Files, BinDir),
    file:set_cwd(Cwd).

do_work({files, FileList, BinDir}) ->
    visit_files(lists:map(binary_to_list(FileList)), BinDir).

add_work({dir, Dir}, BinDir) ->
    simple_send(add_work, {dir, list_to_binary(Dir), BinDir });

add_work({files, FileList}, BinDir) ->
    add_filtered_files(FileList, [], BinDir).

%lists:map(fun(X)->list_to_binary(X) end, FileList) }).                    
add_filtered_files([], Files, BinDir) -> simple_send(add_work, {files, Files, BinDir});
add_filtered_files([ H | T ], Files, BinDir) ->
    case filelib:is_dir(H) of 
        true -> io:format("Adding Directory ~p~n", [H]),
                add_work({dir, H}, BinDir),
                add_filtered_files(T, Files, BinDir);
        false -> add_filtered_files(T, [ list_to_binary(H) | Files ], BinDir)
    end.


%% visit_files(FileList) ->
%%     case lists:split(?MAXLEN, FileList) of
%%         { List1, [] }-> visit_list(List1);
%%         { List1, List2 } -> add_work({files, List1}),
%%                             visit_files(List2)
%%     end.
visit_files(FileList, Tag) ->
    case longer(?MAXLEN, FileList) of
        true  -> {List1, List2} = lists:split(?MAXLEN, FileList),
                 add_work({files, List1}, Tag),
                 visit_files(List2, Tag);
        false -> visit_list(FileList, Tag)
    end.
            

visit_list([], Tag) -> simple_send(work_done, Tag);
visit_list([H | T], Tag) -> 
    case filelib:is_dir(H) of 
        true -> io:format("Adding Directory ~p~n", [H]),
                add_work({dir, H}, Tag),
                visit_list(T, Tag);
        false -> io:format("Visiting ~p~n", [H])
    end.
