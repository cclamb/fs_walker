%%% This is not quite right either ---
-module(fsw_walker).

-behaviour(gen_server). 
%% API 

-export([start_link/1,
         start/3,
         stop/0 ]).

%% gen_server callbacks 
-export([init/1, 
         terminate/2,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3
        ]         ).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc Starts the server.
%%
%% @spec start_link(Port::integer()) -> {ok, Pid}
%% where %% Pid = pid()
%% @end
%%--------------------------------------------------------------------
start_link(Logfile) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Logfile], []).

%%--------------------------------------------------------------------
%% @doc Stops the server.
%% @spec stop() -> ok
%% @end
%%-------------------------------------------------------------------- 
stop() ->
    gen_server:cast(?SERVER, stop).

init([Root, ClientArg]) ->
    {ClientCount, _X} = string:to_integer(ClientArg),
    fsw_eventlog:info_msg("The client count is ~w~n", [ClientCount]),
    fsw_eventlog:info_msg("The root is ~w~n", [Root]),
    start(Root, "random.log", ClientCount),
    {ok, [], 0}.

start_logs() ->
    ok.

%% Watch out --- the root here is a stirng, 
start(Root, Logfile, ClientCount) ->
    fsw_eventlog:start_link(Logfile),
    fsw_blackboard:start_link([Root]),
    spawn_clients(visitor_callback, ClientCount, ClientCount).

spawn_clients(_Callback, 0, _MaxClients) -> ok;

spawn_clients(Callback, N, MaxClients) ->
    fsw_worker:start([Callback, N, MaxClients]),
    spawn_clients(Callback, N-1, MaxClients).

handle_call(_X, _From, State) ->    
    {stop, ok, State}.

handle_cast(_X, State) ->
    {stop, ok, State}.

handle_info(_X, State) ->
    {stop, ok, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.




   

    
