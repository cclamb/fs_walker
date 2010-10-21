-module(fs_walker).
-export([start/2, init/1]).

init([Root, ClientArg]) ->
   {ClientCount, _X} = string:to_integer(ClientArg),
   io:format("The client count is ~w~n", [ClientCount]),
   io:format("The root is ~w~n", [Root]),
   start(Root, ClientCount).

start(Root, ClientCount) ->
    error_logger:logfile({open, "fs_walker_log.log"}),
    error_logger:tty(false),
    fs_event:start_link(),
    fs_event_logger:add_handler(),
    fs_server:start_link(),
    spawn_clients(visitor_callback, ClientCount, ClientCount),
    fs_server:set_root(Root).

spawn_clients(_Callback, 0, _MaxClients) -> ok ;

spawn_clients(Callback, N, MaxClients) ->
    fs_visitor:start([Callback, N, MaxClients]),
    spawn_clients(Callback, N-1, MaxClients).
   

    
