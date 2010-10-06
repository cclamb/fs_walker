% Workload server
-module(fs_server).
-author("raballa@sandia.gov").
-vsn("0.1.0").

% Want gen_server behaviour....
-behaviour(gen_server).

% gen_server call-backs
-export([init/1, handle_call/3, handle_cast/2, 
	 handle_info/2, terminate/2, code_change/3]).

-define(SERVER, work_server).

-export([start/0, stop/1, get_work/0, add_work/1, work_complete/2,
         status/0, process_died/1]).

start() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop(_State) -> ok.

% work is either {dir, directoryname} or
%  {files, list-of-files}
%
%
% Calls: get_work -> pkg | no_work
%        add_dir directory -> ok
%        add_files ?? -> ok
%        work_done pkg -> ok


% How do we represetn inprogress, and what to do on failure of client that
% has the work?
-record(state, {todo =[], inprogress=[], visited=0, dir_errors=0, file_errors=0 }).

is_done(#state{todo=[], inprogress=[]}) ->   true;
is_done(_) -> false.

init([]) ->{ok, #state{}}.

% Synchronous calls...
get_work() ->  gen_server:call(?SERVER, {get_work}).
status() ->  gen_server:call(?SERVER, status).

work_complete(Pkg, FilesVisited) ->
    gen_server:call(?SERVER, {work_complete, Pkg, FilesVisited}).

% These next elements should be casts, I think...
add_work(Pkg) ->
    gen_server:cast(?SERVER, {add_work, Pkg}).

% Called from Error Handler
process_died(Pid) ->  gen_server:cast(?SERVER, {pid_died, Pid}).

% This is a synchronous call, and should never show up more than once at  time
% from a given PID.
%
handle_call({get_work}, {FromPid, _FromTag} , State)->
    case State#state.todo of
        [ H | T ] -> Reply = {visit, H},
                     NewState = State#state{todo = T,
                                            inprogress = [{FromPid, H} |
                                                          State#state.inprogress]},
                     

                     {reply, Reply, NewState};
        _ ->    {reply, {no_work}, State}
    end;

handle_call({work_complete, Pkg}, From, State) ->
    % Need to remove Pkg from inprogress list
    case lists:keymember(Pkg, 2, State#state.inprogress) of
        true -> {reply,
                 ok,
                 State#state{inprogress = lists:keydelete(Pkg, 2,
                                                          State#state.inprogress)}};
        
        false -> fs_event:spurious_complete(From),
                 {reply, error, State}
    end;

handle_call(status, _From, State) ->
    io:format("todo = ~p; in_progress = ~w~n",
              [State#state.todo, State#state.inprogress]),
    {reply, ok, State}.

%
handle_cast({add_work, Pkg}, State) ->
    fs:event:add_work(Pkg),
    {noreply, State#state{todo = [ Pkg | State#state.todo ]}};

handle_cast({pid_died, Pid}, State) ->
    case lists:keymember(Pid, 1, State#state.inprogress) of
        true ->
            % need to pull out the package?
            case lists:keysearch(Pid, 1, State#state.inprogress) of
                {Pid, Pkg} ->
                    State1 = State#state{inprogress=lists:keydelete(Pid, 1,
                                                                    State#state.inprogress)},
                    
                    % Push back into the ToDo list
                    {noreply, State1#state{todo=[Pkg, State#state.todo]}};
                _  -> fs_event:orphan_died(),
                      {noreply, State}
            end;
        false  -> fs_event:orphan_died(Pid),
                  {noreply, State}
    end;


handle_cast(_Msg, State) ->
     {noreply, State}.


handle_info(_Info, State) ->
     {noreply, State}.

terminate(_Reason, _State) ->
     ok.

code_change(_OldVsn, State, _Extra) ->
     {ok, State}.

        

    
