%%% Supervisor module, designed to watch over M distributed nodes
%%% Each client will itself be a supervisor_bridge to the worker class.
%%% 

%% This supervisor needs to call a module that, on init, calls back and dynamically 
%% adds the right number of children.

%% See the simple call0-back stuff in the supervisor behaviour.

-module(fsw_distributed_node_sup).

-behavior(supervisor).

-export([start_link/3]).
-export([init/1]).
-export([spawn_nodes/3]).

%% Froeach node in node list, create distributed node instance.
start_link(NodeList, ClientCount, CallBack) ->
    supervisor:start_link(?MODULE, [NodeList, ClientCount, CallBack]).

init([NodeList, ClientCount, Callback]) ->
    {ok, {{one_for_one, 1, 60},
          [
           {?MODULE, {?MODULE,  spawn_nodes, [1, 2, 4]},
            permanent, brutal_kill, worker, [?MODULE]}]}}.

build_child_list([], _ClientCount, _Callback, L) ->  L;
build_child_list([H | T], ClientCount, Callback, L) ->
    Name = foo,
    Clause = {Name, {fsw_rnode_sup, start_link, [H, ClientCount, Callback]},
              permanent, 2000, supervisor, [fsw_remote_node_sup]},
    build_child_list(T, ClientCount, Callback,  [Clause | L]).


spawn_nodes(NodeList, ClientCount, Callback) ->
    fsw_eventlog:info_msg("~s: spawn_nodes called; clients = ~w~n", [?MODULE, ClientCount]).
%lists:map(fun(X) -> spawn_link(X, fsw_node_sup, init, [ClientCount]).
