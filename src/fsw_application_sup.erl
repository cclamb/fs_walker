%%% OTP module -- supervisor bridge to supervise workers
%% When worker dies, worker_supervisor can tell work server of its death.

%OBSOLETE
-module(fsw_application_sup).
-behavior(supervisor_bridge).

-export([start_link/3]).
-export([init/1, terminate/2]).

start_link(Node, SupervisorName, ApplicationName) ->
%%    error_logger:info_msg("App Sup startup~wn", [Node, ApplicationName]),       
    supervisor_bridge:start_link(SupervisorName,  [Node, ApplicationName]).

init([Node, ApplicationName]) ->
%%    error_logger:info_msg("App Sup INIT ~w~n", [Node, ApplicationName]),       
    Pid = proc_lib:spawn_link(Node, application, start, [ApplicationName]),
    error_logger:info_msg("App Sup INIT spawn=~w~n", [Pid]),       
    {ok, Pid, Pid}.

terminate(Reason, _oState) ->
    case Reason of 
        shutdown -> ok;
        _Other  ->  ok
    end.
