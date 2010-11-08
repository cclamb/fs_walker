-module(fsw_server_sup).
-behavior(supervisor).
-export([start_link/0, init/1]).

%% API -export([start_link/0]).
%% Supervisor callbacks -export([init/1]).
-define(SERVER, ?MODULE).

start_link() ->
    fsw_eventlog:info_msg("Got to server_sup:start_link called~n", []),       
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init(_Args) ->
    %% Bob --- need to get directory, clients, nodes from here, or in config file. 
    error_logger:info_msg("Got to init~n", []),   
    {ok, LogFile} = application:get_env(logfile),
    {ok, Root} = application:get_env(directory),
    {ok, UseTTY} = application:get_env(show_tty),

    %% Spawn the event logger
    EventLogger = fsw_eventlog:server_name(),
    EventLog = {fsw_eventlog, {gen_event, start_link, [EventLogger]},
                permanent, 2000, worker, [gen_event]},
    
    %% Spawn the server
    Server = {fsw_blackboard, {fsw_blackboard, start_link, [LogFile, UseTTY,
                                                            [Root]]},
              permanent, 2000, worker, [fsw_blackboard]},
    
    Children = [EventLog, Server],
    RestartStrategy = {one_for_one, 0, 1},
    {ok, {RestartStrategy, Children}}.
