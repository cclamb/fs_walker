-module(file_work_server).
-author("raballa@sandia.gov").
-vsn("0.1.0").
% Want gen_server behaviour....
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([make_dir_pkg/1, is_done/1, get_worklist/1]).
% work is either {dir, directoryname} or
%  {files, list-of-files}
%
%
% Calls: get_work -> pkg | no_work
%        add_dir directory -> ok
%        add_files ?? -> ok
%        work_done pkg -> ok




make_pkg(Dir) -> {dir, list_to_binary(Dir)}.
make_file_pkg(Files) -> {files, lists:map(list_to_binary, Files) }.

get_worklist({dir, BinDir}) -> [binary_to_list(BinDir)];
get_worklist({files, BinList}) -> lists:map(binary_to_list, BinList).

% Want this to be a list of lists... and make the strings into binaries?
%make_file_pkg(Files) -> .
     
-record(server_state, {todo =[], inprogress=[], visited=0, errors=0 }).

is_done(#server_state{todo=[], inprogress=[]}) ->
    true;
is_done(_) -> false.

init([]) ->{ok, #server_state{}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
     {noreply, State}.

handle_info(_Info, State) ->
     {noreply, State}.

terminate(_Reason, _State) ->
     ok.

code_change(_OldVsn, State, _Extra) ->
     {ok, State}.
    

        

    
