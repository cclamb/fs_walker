%%% Supervisor module, designed to watch over N clients
%%% on a single distributed node.
%%% Each client will itself be a supervisor_bridge to the worker class.
%%% 
%%% Question --- how do we handle the monitoring to notify the blackboard that
%%% a client has died?
-module(fsw_node_sup).
-behavior(supervisor).

-export([start_link/3]).
-export([init/1]).

%% Start the workers in the node.
start_link(_Node, ClientCount, Callback) ->
    error_logger:info_msg("fsw_node_sup: ~w/~w~n", [ClientCount, Callback]),
    supervisor:start_link(?MODULE, [ClientCount, Callback]).

init([ClientCount, Callback]) ->
    error_logger:info_msg("fsw_node_sup:init ~w/~w~n", [ClientCount, Callback]),
    Children = build_child_list(ClientCount, Callback, ClientCount, []),
    {ok, {{one_for_one, 0, 60},   Children}}.

build_child_list(0, _Callback, _Max, L) ->  L;
build_child_list(N, Callback, Max, L) ->
    Name = list_to_atom(lists:concat(["fsworker_", N])),
    Clause = {Name, {fsw_worker_sup, start_link, [Callback, N, Max]},
              permanent, 2000, supervisor, [fsw_worker_sup]},
    build_child_list(N-1, Callback, Max, [Clause | L]).

