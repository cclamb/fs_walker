-module(fsw_client_sup).
-behavior(supervisor).
-export([start_link/0, init/1]).

%% API -export([start_link/0]).
%% Supervisor callbacks -export([init/1]).
-define(SERVER, ?MODULE).

start_link() ->
    fsw_eventlog:info_msg("Client supervsior: start_link called~n", []),       
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).
    
init(_Args) ->
    {ok, MasterNode} = application:get_env(master_node),
    fsw_eventlog:info_msg("Client supervsior: master is ~w~n", [MasterNode]),       
    net_adm:ping(MasterNode),
    global:sync(),

    %% this value comes from the fs_client.app file, and is
    %% not overridden from fsw.config
    {ok, ClientInt} = application:get_env(clients_per_node),
    {ClientCount,  _Stuff} = string:to_integer(ClientInt),
    {ok, CallbackModule} = application:get_env(callback_module),

    NodeMod = fsw_node_sup,
    NodeMgr = {NodeMod, {NodeMod,
                         start_link,
                         [node(), ClientCount, CallbackModule]},
               permanent, 2000, worker, [NodeMod]},

    Children = [NodeMgr],
    RestartStrategy = {one_for_one, 0, 1},
    {ok, {RestartStrategy, Children}}.

