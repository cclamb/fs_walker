-module(fsw_master_sup).
-behavior(supervisor).
-export([start_link/0, init/1]).

%% API -export([start_link/0]).
%% Supervisor callbacks -export([init/1]).
-define(SERVER, ?MODULE).

start_link() ->
%%    fsw_eventlog:info_msg("Got to app_sup:start_link called~n", []),       
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

build_workers() ->
    {ok, IncludeMaster} = application:get_env(master_node_is_client),
    {ok, NodeListFile} = application:get_env(nodelist_file),
    case file:consult(NodeListFile) of
        {ok, [[]]} ->   [node()];
        {ok, [Workers]} ->
            case IncludeMaster of
                true -> Workers ++ [node()];
                false  -> Workers
            end;
        {error, Reason} ->
            error_logger:error_msg("Node list not defined: ~w; defaults to [~w]~n", [Reason, node()]),
            [node()]
    end.


init(_Args) ->
%%    error_logger:info_msg("MASTER_SUP_INIT~n", []),
    Workers = build_workers(),
    Server = {fsw_master_server,
              {fsw_master_server, start_link, [Workers]},
              permanent, 2000, worker, [fsw_master_server]},
    RestartStrategy = {one_for_one, 0, 1},
    {ok, {RestartStrategy, [Server]}}.

