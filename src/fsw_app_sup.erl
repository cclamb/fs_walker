-module(fsw_app_sup).
-behavior(supervisor).
-export([start_link/0, init/1]).

%% API -export([start_link/0]).
%% Supervisor callbacks -export([init/1]).
-define(SERVER, ?MODULE).

start_link() ->
    io:format("Got to app_sup:start_link called~n", []),       
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init(_Args) ->
    %% Bob --- need to get directory, clients, nodes from here, or in config file. 
    io:format("Got to init~n", []),   
    {ok, Logfile} = application:get_env(logfile),
    {ok, Root} = application:get_env(directory),
    {ok, ClientInt} = application:get_env(clients_per_node),
    {ok, CallbackModule} = application:get_env(callback_module),
    {ok, NodeList } = application:get_env(nodelist),

    {ClientCount,  _Stuff} = string:to_integer(ClientInt),
    io:format("Got logfile ~s~n", [Logfile]),
    io:format("Got Root ~s~n", [Root]),
    io:format("Got Client Count ~w~n", [ClientCount]),
    io:format("Got Callback module ~w~n", [CallbackModule]),
    io:format("Got Node list ~p~n", [NodeList]),
    
    %% Spawn the event logger
    EventLog = {fsw_eventlog, {fsw_eventlog, start_link, [Logfile]},
                permanent, 2000, worker, [fsw_eventlog]},
    
    %% Spawn the server
    Server = {fsw_blackboard, {fsw_blackboard, start_link, [[Root]]},
              permanent, 2000, worker, [fsw_blackboard]},
    
%%     %% spawn the node-managers --- this may need to be some sort of 
%%     %% dynamic creation in the long run --- cause you need to spawn-link
%%     %% across nodes.
%%     %%
%%     %% Actuall --- this guy (fsw_node_sup) needs to dynamically spawn_link
%%     %% to the individual nodes
%%     %% So this node has a supervisor across the distributed nodes
%%     %% and each distributed node is a supervisor for several clients.
    
%%     %%  We really need a fsw_distributed_nodes_sup here

%%     NodeMgr = {fsw_node_sup, {fsw_node_sup,
%%                               start_link, [ClientCount, CallbackModule]},
%%                permanent, 2000, worker, [fsw_node_sup]},

    NodeMgr = {fsw_rnode_sup, {fsw_rnode_sup,
                               start_link,
                               [worker@s919538, ClientCount, CallbackModule]},
               permanent, 2000, worker, [fsw_remote_node_sup]},
    
%%     Children = [EventLog, Server, NodeMgr],
    RestartStrategy = {one_for_one, 0, 1},
    %%    {ok, {RestartStrategy, Children}}.
    {ok, {RestartStrategy, [EventLog, Server, NodeMgr]}}.
