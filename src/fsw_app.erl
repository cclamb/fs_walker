-module(fsw_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_Type, _StartArgs) ->
    case fsw_app_sup:start_link() of
        {ok, Pid} ->
            {ok, Pid};
        Other -> 
            {error, Other}
    end.

stop(_State) ->
    fsw_eventlog:info_msg("fsw_app: Stopped ~n", []),
    init:stop(),
    ok.

    
