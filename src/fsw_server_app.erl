-module(fsw_server_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_Type, _StartArgs) ->
    case fsw_server_sup:start_link() of
        {ok, Pid} ->
            {ok, Pid};
        Other -> 
            {error, Other}
    end.

stop(_State) ->
    fsw_eventlog:info_msg("fsw_server: Stopped ~n", []),
    init:stop(),
    ok.

    
