-module(fsw_test).

-behaviour(gen_server). 
%% API 

-export([start_link/2,
         stop/0 ]).

%% gen_server callbacks 
-export([init/1, 
         terminate/2,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3
        ]         ).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc Starts the server.
%%
%% @spec start_link(Port::integer()) -> {ok, Pid}
%% where %% Pid = pid()
%% @end
%%--------------------------------------------------------------------
start_link(Logfile, Root) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Logfile, Root], []).


%%--------------------------------------------------------------------
%% @doc Stops the server.
%% @spec stop() -> ok
%% @end
%%-------------------------------------------------------------------- 
stop() ->
    gen_server:cast(?SERVER, stop).

init([LogFile, Root]) ->
    error_logger:info_msg("inside fsw_test: logfile is ~s~n", [LogFile]),
    error_logger:info_msg("inside fsw_test: root is ~s~n", [Root]),
    {ok, [], 0}.

handle_call(_X, _From, State) ->    
    {stop, ok, State}.

handle_cast(_X, State) ->
    {stop, ok, State}.

handle_info(_X, State) ->
    {stop, ok, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
