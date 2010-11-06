-module(fsw_client_sup).
-behavior(supervisor).
-export([start_link/0, init/1]).

%% API -export([start_link/0]).
%% Supervisor callbacks -export([init/1]).
-define(SERVER, ?MODULE).

start_link() ->
    fsw_eventlog:info_msg("Got to app_sup:start_link called~n", []),       
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).
    
init(_Args) ->
    fsw_eventlog:info_msg("Got to init~n", []),   
    {ok, LogFile} = application:get_env(logfile),
    {ok, Root} = application:get_env(directory),
    {ok, ClientInt} = application:get_env(clients_per_node),
    {ok, CallbackModule} = application:get_env(callback_module),
    {ok, NodeList} = application:get_env(nodelist),

    {ClientCount,  _Stuff} = string:to_integer(ClientInt),

    contact_nodes(NodeList),    

%%     %% spawn the node-managers --- this may need to be some sort of 
%%     %% dynamic creation in the long run --- cause you need to spawn-link
%%     %% across nodes.
%%     %%
%%     %% Actuall --- this guy (fsw_node_sup) needs to dynamically spawn_link
%%     %% to the individual nodes
%%     %% So this node has a supervisor across the distributed nodes
%%     %% and each distributed node is a supervisor for several clients.
    
%%     %%  We really need a fsw_distributed_nodes_sup here

    NodeMod = fsw_node_sup,
    NodeMgr = {NodeMod, {NodeMod,
                         start_link,
                         [worker@s919538, ClientCount, CallbackModule]},
               permanent, 2000, worker, [NodeMod]},

    Children = [NodeMgr],


    RestartStrategy = {one_for_one, 0, 1},
    {ok, {RestartStrategy, Children}}.

contact_nodes([])->
    error_logger:info_msg("contact nodes: ~w~n", [nodes()]),    
    ok;

contact_nodes([Node | Rest])->
    error_logger:info_msg("Ping to ~w~n", [Node]),
    case net_adm:ping(Node) of
        pong -> 
            contact_nodes(Rest);
        Other  -> error_logger:error_msg("Unable to contact node ~w~n", [ Node ]),
                   {error, Other}
    end.
