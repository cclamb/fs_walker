% Module based on Erlang and OTP/sc_event (page 180)
-module(fsw_eventlog).

-export([ % start_link/0,init/1,
          add_handler/2, delete_handler/2,
          server_name/0,
          work_added/1, spurious_complete/1,
          file_error/2, directory_error/1,
          access_timeout/1, orphan_died/0, orphan_died/1,
          worker_died/1, visit_update/1,
          client_count/1, node_count/1,
          info_msg/2, warning_msg/2, error_msg/2,
          call/2]).

-define(SERVER, ?MODULE).

server_name() ->  {global, ?SERVER}.
    
add_handler(Handler, Args) ->
    gen_event:add_handler({global, ?SERVER}, Handler, Args).

delete_handler(Handler, Args) ->
    gen_event:delete_handler({global, ?SERVER}, Handler, Args).

% Called from server code.
% orphan died - may be called back from process died. Need to keep them separate
orphan_died() ->  gen_event:notify({global, ?SERVER}, {orphan_died}).
orphan_died(Pid) ->    gen_event:notify({global, ?SERVER}, {orphan_died, Pid}).
spurious_complete(Pid)  ->   gen_event:notify({global, ?SERVER}, {orphan_work, Pid}).

% Called from worker code
work_added(Pkg) ->  gen_event:notify({global, ?SERVER}, {work_added, Pkg}).

worker_died(Pid) ->  gen_event:notify({global, ?SERVER}, {process_died, Pid}).

file_error(Filename, Reason) ->
    gen_event:notify({global, ?SERVER}, {file_error, Filename, Reason}).

directory_error(Filename) ->  gen_event:notify({global, ?SERVER}, {directory_error, Filename}).

access_timeout(Filename) ->  gen_event:notify({global, ?SERVER}, {fs_timeout, Filename}).

visit_update(Count) ->  gen_event:notify({global, ?SERVER}, {visited, Count}).

client_count(Count) ->  gen_event:notify({global, ?SERVER}, {client_count, Count}).
node_count(Count) ->  gen_event:notify({global, ?SERVER}, {node_count, Count}).

info_msg(Msg, Args) ->  gen_event:notify({global, ?SERVER}, {info, Msg, Args}).
warning_msg(Msg, Args) ->  gen_event:notify({global, ?SERVER}, {warning, Msg, Args}).
error_msg(Msg, Args) ->  gen_event:notify({global, ?SERVER}, {error, Msg, Args}).

call(Handler, Request) ->
     gen_event:call({global, ?SERVER}, Handler, Request).

