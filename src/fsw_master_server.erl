%% Workload server
-module(fsw_master_server).
-author("raballa@sandia.gov").
-vsn("0.2.0").

% Want gen_server behaviour....
-behaviour(gen_server).

% gen_server call-backs
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

-export([start_link/1, stop/1, client_finished/1, print_run_information/6]).

-define(SERVER, ?MODULE).

%%% How do we represent inprogress, and what to do on failure of client that
%%% has the work?
-record(state, {server, clients, start_time, end_time, workers=[]}).

start_link(WorkerNodes) ->
    %% Debug stub!
%    error_logger:info_msg("master: start_link!~n", []),
    gen_server:start_link({global, ?SERVER}, ?MODULE, [WorkerNodes],[]).

start_client(Node) ->
    {Node, spawn_link(Node,  application, start, [fs_client]), fs_client}.

init([WorkerNodes]) ->
    %% Set the todo list to a list of directories.

    Server = {node(), spawn_link(node(), application, start, [fs_server]), fs_server},
    Clients = lists:map(fun start_client/1, WorkerNodes),
    %% set the start time?
    {ok, #state{server=Server, clients=Clients, start_time= erlang:now(), workers=WorkerNodes}}.

stop(_State) -> ok.

client_finished(Node) -> gen_server:call({global, ?SERVER}, {client_finished, Node}).

handle_call({client_finished, Node}, _From, State) ->
    %% Need to remove Node from inprogress list
    fsw_eventlog:info_msg("Master server gets client finished from ~w~n", [Node]),
    case lists:keymember(Node, 1, State#state.clients) of
        true ->
            case lists:keydelete(Node, 1, State#state.clients) of
                [] ->
                    %% Done here!
                    fsw_eventlog:info_msg("Server: client list EMPTY~n", []),
                    {stop, normal, ok, State#state{clients = [], end_time=erlang:now()}};
                L ->
                    fsw_eventlog:info_msg("Server: client list = ~w~n", [L]),
                    {reply,
                     ok,
                     State#state{clients = L}}
            end;
        
        false -> fsw_eventlog:spurious_complete(Node),
                 {reply, error, State}
    end.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(Reason, State) ->
    fsw_eventlog:info_msg("Terminating due to ~w~n", [Reason]),
    T1 = State#state.start_time,
    T2 = State#state.end_time,
    Visited = fsw_eventlog_handler:files_visited(),
    ClientCt = fsw_eventlog_handler:client_count(),
    NodeCt =  length(State#state.workers),
    FileErrs =  fsw_eventlog_handler:file_errors(),
    DirErrs =  fsw_eventlog_handler:directory_errors(),

    print_run_information(Visited,
                          NodeCt, ClientCt,
                          timer:now_diff(T2, T1),
                          FileErrs, DirErrs),
    ok.


iso_8601_fmt(DateTime) ->
    {{Year,Month,Day},{Hour,Min,Sec}} = DateTime,
    io_lib:format("~4.10.0B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B",
        [Year, Month, Day, Hour, Min, Sec]).
        

print_run_information(FilesVisited, NodeCt, ClientCt, Time, FileErrs, DirErrs) ->
    fsw_eventlog:warning_msg("Time elapsed for ~w files (~w nodes, ~w clients/node) is  ~w~ (~w file/~w dir) errors~n",
                                [ FilesVisited,  NodeCt, ClientCt, Time,   FileErrs, DirErrs  ]),    
    io:format("Time elapsed for ~w files (~w nodes, ~w clients/node) is  ~w (~w file/~w dir) errors~n",
                        [ FilesVisited,  NodeCt, ClientCt, Time,   FileErrs, DirErrs  ]),    

    FileName = "runs.csv",
    case file:open(FileName, [append]) of
        {ok, IoDev} -> 
              Now = iso_8601_fmt(erlang:localtime()),
              io:format(IoDev, "~s,~w,~w,~w,~w~n",
                             [ Now, NodeCt, ClientCt, FilesVisited, Time ]),    
              file:close(IoDev);
        {error, Reason} -> 
              io:format("Errror: Unable to open run log ~s (~s)~n", [FileName, Reason])
    end.


code_change(_OldVsn, State, _Extra) ->
     {ok, State}.

