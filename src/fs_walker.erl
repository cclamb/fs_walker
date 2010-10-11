-module(fs_walker).
-export([start/2]).

start(Root, ClientCount) ->
    fs_event:start_link(),
    fs_event_logger:add_handler(),
    fs_server:start_link(),
    spawn_clients(visitor_callback, ClientCount),
    fs_server:set_root(Root).

spawn_clients(_Callback, 0) -> ok ;

spawn_clients(Callback, N) ->
    fs_visitor:start([Callback, N]),
    spawn_clients(Callback, N-1).
   

    
