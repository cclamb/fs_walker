-module(fs_walker).
-export([start/2]).

start(Root, ClientCount) ->
    fs_event:start_link(),
    fs_event_logger:add_handler(),
    fs_server:start_link(),
    spawn_clients(visitor_callback, ClientCount, ClientCount),
    fs_server:set_root(Root).

spawn_clients(_Callback, 0, _MaxClients) -> ok ;

spawn_clients(Callback, N, MaxClients) ->
    fs_visitor:start([Callback, N, MaxClients]),
    spawn_clients(Callback, N-1, MaxClients).
   

    
