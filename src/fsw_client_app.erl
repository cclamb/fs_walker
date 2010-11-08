-module(fsw_client_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_Type, _StartArgs) ->
    case fsw_client_sup:start_link() of
        {ok, Pid} ->   {ok, Pid};
        Other ->       {error, Other}
    end.

stop(_State) ->
    error_logger:info_msg("fsw_client on ~w stopped ~n", [node()]), 
    fsw_master_server:client_finished(node()),
    ok.

    
