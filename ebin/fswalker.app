%% -*- mode: Erlang; fill-column: 75; comment-column: 50; -*-
{application, fswalker,
 [{description, "File System Walker"},
  {vsn, "0.1.0"},
  {modules, [
             fsw_app,
             fsw_app_sup,
             fsw_blackboard,
%             fsw_distributed_node_sup,
             fsw_eventlog,
             fsw_eventlog_handler,
%             fsw_node_sup,
             fsw_toplevel
%             fsw_worker,
%             fsw_worker_sup,
%             visitor_callback
            ]},
  {registered, [fsw_blackboard]},
  {applications, [kernel, stdlib]},
  {env, [ {logfile, "fsw.log"},
          {clients_per_node, "2"},
          {directory, "/Users/raballa/Documents/"},
          {callback_module, visitor_callback} ]},
  {mod, {fsw_app, [] }}
 ]}.
