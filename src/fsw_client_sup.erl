-module(fsw_client_sup).
-behavior(supervisor).
-export([start_link/0, init/1]).

%% API -export([start_link/0]).
%% Supervisor callbacks -export([init/1]).
-define(SERVER, ?MODULE).

start_link() ->
%%    fsw_eventlog:info_msg("Client supervsior: start_link called~n", []),       
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).
    
init(_Args) ->
    {ok, MasterNode} = application:get_env(master_node),
%%    fsw_eventlog:info_msg("Client supervsior: master is ~w~n", [MasterNode]),       
    %% Make sure we can communicate with the master node.
    net_adm:ping(MasterNode),
    global:sync(),

    %% this value comes from the fs_client.app file,
    {ok, ClientInt} = application:get_env(clients_per_node),
    fsw_eventlog:warning_msg("Clients per node is ~p~n", [ClientInt]),
    {ClientCount,  _Stuff} = string:to_integer(ClientInt),
    fsw_eventlog:warning_msg("Client  count is is ~w~n", [ClientInt]),

    {ok, CallbackModule} = application:get_env(callback_module),
    fsw_eventlog:client_count(ClientCount),

    Children = build_child_list(ClientCount, CallbackModule),
    RestartStrategy = {one_for_one, 10, 60},
    {ok, {RestartStrategy, Children}}.


%% Create the list of child callbacks...
%% This next function is just an implementation-hiding wrapper
build_child_list(ClientCount, Callback) ->
     build_child_list(ClientCount, Callback, ClientCount, []).

build_child_list(0, _Callback, _Max, L) ->  L;
build_child_list(N, Callback, Max, L) ->
    Name = list_to_atom(lists:concat(["fsworker_", N])),
    Clause = {Name, {fsw_worker_sup, start_link, [Callback, N, Max]},
              permanent, infinity, supervisor, [fsw_worker_sup]},
    build_child_list(N-1, Callback, Max, [Clause | L]).
