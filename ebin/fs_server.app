%% -*- mode: Erlang; fill-column: 75; comment-column: 50; -*-
{application, fs_server,
 [{description, "File System Walker"},
  {vsn, "0.1.0"},
  {modules, [
             fsw_server_app,
             fsw_server_sup,
             fsw_blackboard,
             fsw_eventlog,
             fsw_eventlog_handler
            ]},
  {registered, [fsw_blackboard, fsw_eventlog]},
  {applications, [kernel, stdlib]},
  {env, [
         {logfile, "fsw.log"},
         {show_tty, false},
         {directory, "."}
        ]},
  {mod, {fsw_server_app, [] }}
 ]}.
