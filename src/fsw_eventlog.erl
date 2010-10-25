% Module based on Erlang and OTP/sc_event (page 180)
-module(fsw_eventlog).

-export([start_link/1, add_handler/2,
         work_added/1, spurious_complete/1,
         file_error/2, directory_error/1,
         access_timeout/1, orphan_died/0, orphan_died/1,
         worker_died/1, visit_update/1,
         info_message/2, warning_message/2, error_message/2]).

-define(SERVER, ?MODULE).

start_link(LogFile) ->
    Res = gen_event:start_link({local, ?SERVER}),
    fsw_eventlog_handler:add_handler(LogFile),
    Res.   

add_handler(Handler, Args) ->
    gen_event:add_handler(?SERVER, Handler, Args).

delete_handler(Handler, Args) ->
    gen_event:delete_handler(?SERVER, Handler, Args).

% Called from server code.
% orphan died - may be called back from process died. Need to keep them separate
orphan_died() ->  gen_event:notify(?SERVER, {orphan_died}).
orphan_died(Pid) ->    gen_event:notify(?SERVER, {orphan_died, Pid}).
spurious_complete(Pid)  ->   gen_event:notify(?SERVER, {orphan_work, Pid}).

% Called from worker code
work_added(Pkg) ->  gen_event:notify(?SERVER, {work_added, Pkg}).

worker_died(Pid) ->  gen_event:notify(?SERVER, {process_died, Pid}).

file_error(Filename, Reason) ->
    gen_event:notify(?SERVER, {file_error, Filename, Reason}).

directory_error(Filename) ->  gen_event:notify(?SERVER, {directory_error, Filename}).

access_timeout(Filename) ->  gen_event:notify(?SERVER, {fs_timeout, Filename}).

visit_update(Count) ->  gen_event:notify(?SERVER, {visited, Count}).

info_message(Msg, Args) ->  gen_event:notify(?SERVER, {info, Msg, Args}).
warning_message(Msg, Args) ->  gen_event:notify(?SERVER, {warning, Msg, Args}).
error_message(Msg, Args) ->  gen_event:notify(?SERVER, {error, Msg, Args}).

