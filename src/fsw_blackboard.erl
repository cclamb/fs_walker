% Workload server
-module(fsw_blackboard).
-author("raballa@sandia.gov").
-vsn("0.2.0").

% Want gen_server behaviour....
-behaviour(gen_server).

% gen_server call-backs
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

-export([start_link/2, stop/1, get_work/0, add_work/1, work_complete/1,
         status/0, process_died/1]).

-define(SERVER, fsw_blackboard).

start_link(LogFile, RootList) ->
    gen_server:start_link({global, ?SERVER}, ?MODULE,
                          [LogFile, RootList], []).

stop(_State) -> ok.

%%% work is either {dir, directoryname} or
%%%  {files, list-of-files}
%%%
%%%
%%% Calls: get_work -> pkg | no_work
%%%        add_dir directory -> ok
%%%        add_files ?? -> ok
%%%        work_done pkg -> ok

%%% How do we represent inprogress, and what to do on failure of client that
%%% has the work?
-record(state, {todo =[], inprogress=[] }).

is_done(#state{todo=[], inprogress=[]}) ->   true;
is_done(_) -> false.

make_dir_pkg(X) ->
    Absdir = filename:absname(X),
    {directory,
     list_to_binary("."),
     list_to_binary(Absdir)}.

init([LogFile, RootList]) ->
    fsw_eventlog_handler:add_handler(LogFile),
    fsw_eventlog:info_msg("Blackboard started with rootlist~n", []),
    %% Set the todo list to a list of directories.
    ToDo = lists:map(fun(X)-> make_dir_pkg(X) end, RootList),
    {ok, #state{todo=ToDo}}.

%%% Synchronous calls...
get_work() ->  gen_server:call({global, ?SERVER}, {get_work}).

status() ->  gen_server:call({global, ?SERVER}, status).

work_complete(Pkg) ->
    gen_server:call({global, ?SERVER}, {work_complete, Pkg}).

%%% These next elements should be casts, I think...
add_work(Pkg) ->
    gen_server:call({global, ?SERVER}, {add_work, Pkg}).

%%% Sets the root directory to an absolute path name, if needed.
%%set_root(Dir) ->
%%   add_work({directory, list_to_binary("."), list_to_binary(filename:absname(Dir))}).

%%% Called from Error Handler
process_died(Pid) ->  gen_server:cast({global, ?SERVER}, {pid_died, Pid}).

%%% This is a synchronous call, and should never show up more than once at  time
%%% from a given PID.
handle_call({get_work}, {FromPid, _FromTag} , State)->
    case State#state.todo of
        [ H | T ] -> Reply = {visit, H},
                     NewState = State#state{todo = T,
                                            inprogress = [{FromPid, H} |
                                                          State#state.inprogress]},
                     
                     {reply, Reply, NewState};
        _ ->
            case State#state.inprogress of
                [] ->
                    %% Nothing in progress or to do
                    fsw_eventlog:info_msg("Returning done ~n", []),
                    {reply, {done}, State};

                [_H | _T ] ->
                    fsw_eventlog:info_msg("Returning no_work ~n", []),
                    {reply, {no_work}, State}
            end

    end;

handle_call({work_complete, Pkg}, From, State) ->
    % Need to remove Pkg from inprogress list
    case lists:keymember(Pkg, 2, State#state.inprogress) of
        true -> {reply,
                 ok,
                 State#state{inprogress = lists:keydelete(Pkg, 2,
                                                          State#state.inprogress)}};
        
        false -> fsw_eventlog:spurious_complete(From),
                 {reply, error, State}
    end;

handle_call(status, _From, State) ->
    fsw_eventlog:info_msg("todo = ~p; in_progress = ~w~n",
              [State#state.todo, State#state.inprogress]),
    {reply, ok, State};

%%%
handle_call({add_work, Pkg}, _From, State) ->
    fsw_eventlog:work_added(Pkg),
    {reply, ok,  State#state{todo = [ Pkg | State#state.todo ]}}.

handle_cast({pid_died, Pid}, State) ->
    case lists:keymember(Pid, 1, State#state.inprogress) of
        true ->
            %% need to pull out the package?
            case lists:keysearch(Pid, 1, State#state.inprogress) of
                {Pid, Pkg} ->
                    State1 = State#state{inprogress=lists:keydelete(Pid, 1,
                                                                    State#state.inprogress)},
                    
                    %% Push back into the ToDo list
                    {noreply, State1#state{todo=[Pkg, State#state.todo]}};
                _  -> fsw_eventlog:orphan_died(),
                      {noreply, State}
            end;
        false  -> fsw_eventlog:orphan_died(Pid),
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

