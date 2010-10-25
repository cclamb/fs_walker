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

init(_Args) ->
    {ok, {{one_for_one, 1, 60},
          [
           {?MODULE, {?MODULE,  spawn_nodes, [1, 2, 4]},
            permanent, brutal_kill, worker, [?MODULE]}]}}.

spawn_nodes(NodeList, ClientCount, Callback) ->
    io:format("~s: spawn_nodes called; clients = ~w~n", [?MODULE, ClientCount]).
%    lists:map(fun(X) -> spawn_link(X, fsw_node_sup, init, [ClientCount]).
