-module(fsw_server_sup).
-behavior(supervisor).
-export([start_link/0, init/1]).

%% API -export([start_link/0]).
%% Supervisor callbacks -export([init/1]).
-define(SERVER, ?MODULE).

start_link() ->
    fsw_eventlog:info_msg("Got to server_sup:start_link called~n", []),       
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init(_Args) ->
    %% Bob --- need to get directory, clients, nodes from here, or in config file. 
    error_logger:info_msg("Got to init~n", []),   
    {ok, LogFile} = application:get_env(logfile),
    {ok, Root} = application:get_env(directory),
    {ok, ClientInt} = application:get_env(clients_per_node),
    {ok, CallbackModule} = application:get_env(callback_module),
    {ok, NodeList} = application:get_env(nodelist),

    {ClientCount,  _Stuff} = string:to_integer(ClientInt),

%%     fsw_eventlog:info_msg("Got logfile ~s~n", [LogFile]),
%%     fsw_eventlog:info_msg("Got Root ~s~n", [Root]),
%%     fsw_eventlog:info_msg("Got Client Count ~w~n", [ClientCount]),
%%     fsw_eventlog:info_msg("Got Callback module ~w~n", [CallbackModule]),
%%     fsw_eventlog:info_msg("Got Node list ~p~n", [NodeList]),
    
    %% Spawn the event logger
    EventLogger = fsw_eventlog:server_name(),
    EventLog = {fsw_eventlog, {gen_event, start_link, [EventLogger]},
                permanent, 2000, worker, [gen_event]},
    
    %% Spawn the server
    Server = {fsw_blackboard, {fsw_blackboard, start_link, [LogFile, [Root]]},
              permanent, 2000, worker, [fsw_blackboard]},
    
    Children = [EventLog, Server],
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
