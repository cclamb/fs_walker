%%% OTP module -- supervisor bridge to supervise workers
%% When worker dies, worker_supervisor can tell work server of its death.
-module(fsw_worker_sup).
-behavior(supervisor_bridge).

-export([start_link/3]).
-export([init/1, terminate/2]).

start_link(CallbackModule, Index, MaxClients) ->
    supervisor_bridge:start_link(fsw_worker_sup,
                                 [CallbackModule, Index, MaxClients]).

init([CallbackModule, Index, MaxClients]) ->
    Pid = spawn_link(fsw_worker, init, [CallbackModule, Index, MaxClients]),
    {ok, Pid, Pid}.

terminate(Reason, State) ->
    case Reason of 
        shutdown -> ok;
        _Other  -> fsw_blackboard:process_died(State),
                   ok
    end.
