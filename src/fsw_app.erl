-module(fsw_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_Type, _StartArgs) ->
    io:format("fsw_app: start~n", []),
    case fsw_app_sup:start_link() of
        {ok, Pid} ->
            io:format("fsw_app:  start_link succeeded~n", []),
            {ok, Pid};
        Other -> 
            io:format("fsw_app: start_link FAILED ~w ~n", [Other]),
            {error, Other}
    end.

stop(_State) ->
    io:format("fsw_app: Stopped ~n", []),
    ok.

    
