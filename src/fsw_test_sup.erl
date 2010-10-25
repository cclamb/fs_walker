-module(fsw_test_sup).
-behavior(supervisor).
-export([start_link/0, init/1]).

%% API -export([start_link/0]).
%% Supervisor callbacks -export([init/1]).
-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    %% Bob
    {ok, Logfile} = application:get_env(logfile),
    {ok, Root} = application:get_env(directory),
    io:format("Got logfile ~s~n", [Logfile]),
    io:format("Got directory ~s~n", [Root]),
    Server = {fsw_test, {fsw_test, start_link, [Logfile, Root]},
              permanent, 2000, worker, [fsw_test]},
    Children = [Server],
    RestartStrategy = {one_for_one, 0, 1},
    {ok, {RestartStrategy, Children}}.
