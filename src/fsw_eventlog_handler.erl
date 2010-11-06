% Module based on Erlang and OTP/sc_event_logger (page 184ff)
-module(fsw_eventlog_handler).
-behaviour(gen_event).
-export([add_handler/1, delete_handler/0]).

-export([init/1, handle_event/2, handle_call/2,
         handle_info/2, code_change/3, terminate/2]).

-record(state, {file_errors=0, directory_errors=0, visited=0, timeouts=0}).

init([LogFile]) ->
    error_logger:logfile({open, LogFile}),
%%    error_logger:tty(false),
    {ok, #state{}}.

add_handler(LogFile) ->
    error_logger:info_msg("Adding handler from ~w~n", [?MODULE]),
    fsw_eventlog:add_handler(?MODULE, [LogFile]).

delete_handler() ->
    fsw_eventlog:delete_handler(?MODULE, []).

handle_event({work_added, Pkg}, State) ->
    error_logger:info_msg("work added(~p)~n", [Pkg]),
    {ok, State};

handle_event({orphan_died}, State) ->
    error_logger:warning_msg("orphan process_died()", []),
    {ok, State};

handle_event({orphan_died, Pid}, State) ->
    error_logger:warning_msg("orphan process_died(~w)", [Pid]),
    {ok, State};

handle_event({process_died, Pid}, State) ->
    error_logger:warning_msg("process_died(~w)", [Pid]),
    fsw_blackboard:process_died(Pid),
    {ok, State};

handle_event({orphan_work, Pid}, State) ->
    error_logger:info_msg("Pid ~p) reports work done, but work not assigned", [Pid]),
    {ok, State#state{file_errors = State#state.file_errors + 1}};

handle_event({file_error, FileName, Reason}, State) ->
    error_logger:warning_msg("file_error(~p):~p", [FileName, Reason]),
    {ok, State#state{file_errors = State#state.file_errors + 1}};

handle_event({directory_error, FileName}, State) ->
    error_logger:info_msg("file_error(~p)", [FileName]),
    {ok, State#state{file_errors = State#state.directory_errors + 1}};

handle_event({fs_timeout, FileName}, State) ->
    error_logger:info_msg("timeout(~p)", [FileName]),
    {ok, State#state{timeouts = State#state.timeouts + 1}};

handle_event({visited, Count}, State) ->
    error_logger:info_msg("~w files visited so far~n",
                              [State#state.visited + Count]),
    {ok, State#state{visited = State#state.visited + Count}};

handle_event({info, Msg, Args}, State) ->
    error_logger:info_msg(Msg, Args),
    {ok, State};

handle_event({warning, Msg, Args}, State) ->
    error_logger:warning_msg(Msg, Args),
    {ok, State};

handle_event({error, Msg, Args}, State) ->
    error_logger:error_msg(Msg, Args),
    {ok, State};

handle_event(Event, State) ->
    error_logger:error_msg("Unknown Event(~w)", [Event]),
    {ok, State}.

%% Stubs for call-backs.
handle_call(_Request, State) ->  {ok, ok, State}.

handle_info(_Info, State) -> {ok, State}.

terminate(_Reason, _State) ->    ok.

code_change(_OldVsn, State, _Extra) ->   {ok, State}.

