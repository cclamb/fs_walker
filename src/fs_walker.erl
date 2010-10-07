-module(fs_walker).
-export([start/1]).

start(Root) ->
    fs_event:start_link(),
    fs_event_logger:add_handler(),
    fs_server:start_link(),
    fs_server:set_root(Root).
    
