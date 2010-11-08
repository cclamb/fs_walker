%%% OTP module -- supervisor bridge to supervise workers
%% When worker dies, worker_supervisor can tell work server of its death.

-module(fsw_rnode_sup).
-behavior(supervisor_bridge).

-export([start_link/3, init/1, terminate/2]).

start_link(Node, Clients, CallbackModule) ->
    error_logger:info_msg("in fsw_rnode_sup:start_link node = ~p/~w/~w~n", [Node, Clients, CallbackModule]),
    supervisor_bridge:start_link(?MODULE, [Node, Clients, CallbackModule]).

init([Node, Clients, CallbackModule]) ->
    % needed?
    error_logger:info_msg(
      "fsw_rnode_sup: Attempting spawn to (~p) from ~p ~n", [Node, node()]),

    Pid = spawn_link(Node,
                     fsw_node_sup,
                     start_link,
                     [Node, Clients, CallbackModule]),
    {ok, Pid, Pid}.

terminate(Reason, State) ->
    case Reason of 
        shutdown ->
            fsw_eventlog:info_msg("SHUTDOWN of fsw_node_sup~n",[]),
            ok;
        _Other  ->
            fsw_eventlog:info_msg(
              "Other death of remote fsw_node_sup:~n",[]),
            ok
    end.
