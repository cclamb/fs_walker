%%% This module is a wrapper to call applcation:start.

%% For some reason, just calling -s application start fswtest 
%% from the erl command line does not quite work. Wrapping it into
%% a module did the trick.

%% I'd prefer to do it from the command line, though!

-module(fsw_toplevel).
-export([start/1, start/0, start_test/0]).

start() ->
     start(fswalker).

start_test() ->
     start(fswtest).

start(AppName)->  application:start(AppName).
