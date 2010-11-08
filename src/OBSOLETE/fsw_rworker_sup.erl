%%% OTP module -- supervisor bridge to supervise workers
%% When worker dies, worker_supervisor can tell work server of its death.
-module(fsw_rworker_sup).
-behavior(supervisor_bridge).

-export([start_link/3]).
-export([init/1, terminate/2]).

start_link(Node, Index, CallbackModule) ->
    supervisor_bridge:start_link({local, fred},
                                 fsw_rworker_sup,
                                 [Node, CallbackModule, Index, Index]).

init([Node, CallbackModule, Index, MaxClients]) ->
    error_logger:info_msg("Trying to spawn worker~n",[]),
    Pid = proc_lib:spawn_link(Node, fsw_worker, init,
                              [CallbackModule, Index, MaxClients]),
    error_logger:info_msg("Ok, able to spawn worker~n",[]),
    {ok, Pid, Pid}.

terminate(Reason, State) ->
    case Reason of 
        shutdown -> ok;
        _Other  -> fsw_blackboard:process_died(State),
                   ok
    end.
