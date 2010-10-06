-module(io_handler).
-export([init/1, terminate/1, handle_event/2]).

-include("event.hrl").

%-record(event, {action, id, alarm, sender}).

init(Count) ->  Count.

terminate(Count) ->
     {count, Count}.

handle_event(#event{action=raise_alarm, id=Id, alarm=Alarm}, Count)->
    print(alarm, Id, Alarm, Count),
    Count+1;

handle_event(#event{action=clear_alarm, id=Id, alarm=Alarm}, Count) ->
    print(clear, Id, Alarm, Count),
    Count - 1;

handle_event(_Event, Count) -> Count.

print(Type, Id, Alarm, Count) ->
    Date = fmt(date()), Time=fmt(time()),
    io:format("#~w, ~s ~s, ~w,~w, ~p~n",
              [Count, Date, Time, Type, Id, Alarm]).

fmt({Aint, Bint, Cint}) ->
    Astr = pad(integer_to_list(Aint)),
    Bstr = pad(integer_to_list(Bint)),
    Cstr = pad(integer_to_list(Cint)),
    [Astr,$:,Bstr,$:,Cstr].

pad({M1}) -> [$0, M1];
pad(Other) -> Other.
    
    
    

