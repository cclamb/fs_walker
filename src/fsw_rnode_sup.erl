%%% OTP module -- supervisor bridge to supervise workers
%% When worker dies, worker_supervisor can tell work server of its death.

-module(fsw_rnode_sup).
-behavior(supervisor_bridge).

-export([start_link/3, init/1, terminate/2]).

start_link(Node, Clients, CallbackModule) ->
    error_logger:info_msg("in fsw_rnode_sup:it node = ~p/~w/~w~n", [Node, Clients, CallbackModule]),
    supervisor_bridge:start_link(?MODULE, [Node, Clients, CallbackModule]).

init([Node, Clients, CallbackModule]) ->
    error_logger:info_msg("fsw_rnode_sup:init node = ~p (~p) ~n", [node(), Node]),
    % needed?
    Pid = proc_lib:spawn_link(Node,
                              fsw_node_sup,
                     start_link,
                     [Clients, CallbackModule]),
    {ok, Pid, Pid}.

terminate(Reason, State) ->
    case Reason of 
        shutdown -> ok;
        _Other  ->
            fsw_eventlog:info_msg("Death of remote fsw_node_sup:process_died~n",[]),
            ok
    end.
