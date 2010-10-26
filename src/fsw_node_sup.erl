%%% Supervisor module, designed to watch over N clients
%%% on a single distributed node.
%%% Each client will itself be a supervisor_bridge to the worker class.
%%% 
%%% Question --- how do we handle the monitoring to notify the blackboard that
%%% a client has died?
%%%
%%%
%%% BOB --- HERE!
-module(fsw_node_sup).
-behavior(supervisor).

-export([start_link/2]).
-export([init/1]).

%% Froeach node in node list, create distributed node instance.
start_link(ClientCount, Callback) ->
    supervisor:start_link(?MODULE, [ClientCount, Callback]).

init([ClientCount, Callback]) ->
    Children = build_child_list(ClientCount, Callback, ClientCount, []),
    io:format("fsw_node_sup: ~w~n", [Children]),
    {ok, {{one_for_one, 0, 60},   Children}}.

build_child_list(0, _Callback, _Max, L) ->  L;
build_child_list(N, Callback, Max, L) ->
    Name = list_to_atom(lists:concat(["fsworker_", N])),
    Clause = {Name, {fsw_worker_sup, start_link, [Callback, N, Max]},
              permanent, 2000, supervisor, [fsw_worker_sup]},
    build_child_list(N-1, Callback, Max, [Clause | L]).

