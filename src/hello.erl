-module(hello).
-export([init/1]).

init(Arg) ->
    error_logger:info_msg("Info message: hello from ~w (~w) ~n",
                 [node(), Arg]).
    
