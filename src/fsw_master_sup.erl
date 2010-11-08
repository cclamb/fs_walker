-module(fsw_master_sup).
-behavior(supervisor).
-export([start_link/0, init/1]).

%% API -export([start_link/0]).
%% Supervisor callbacks -export([init/1]).
-define(SERVER, ?MODULE).

start_link() ->
    fsw_eventlog:info_msg("Got to app_sup:start_link called~n", []),       
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

build_workers() ->
    {ok, Workers} = application:get_env(nodelist),
    {ok, IncludeMaster} = application:get_env(master_node_is_client),
    case IncludeMaster of
        true -> Workers ++ [node()];
        false  -> Workers
    end.
    
init(_Args) ->
    error_logger:info_msg("MASTER_SUP_INIT~n", []),
    Server = {fsw_master_server,
              {fsw_master_server, start_link, [build_workers()]},
              permanent, 2000, worker, [fsw_master_server]},
    RestartStrategy = {one_for_one, 0, 1},
    {ok, {RestartStrategy, [Server]}}.

