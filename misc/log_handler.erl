-module(log_handler).
-export([init/1, terminate/1, handle_event/2]).
-include(event).

init(File) ->
    {ok, Fd} = file:open(File,write),
    Fd.

terminate(Fd) ->
    file:close(Fd).

handle_event(#event(action=Action, id=Id, alarm=Alarm}, Fd) ->
    {MegaSec, Sec, MicroSec} = now(),
    Args = io:format(Fd, "~w,~w,~w,~w,~w,~p~n",
                     [MegaSec, Sec, MicroSec, Action, Id, Alarm]),
    Fd;

handle_event(_, Fd) -> Fd.

    

