%% -*- mode: Erlang; fill-column: 75; comment-column: 50; -*-
{application, fs_client,
 [{description, "File System Walker Client App"},
  {vsn, "0.3.0"},
  {modules, [
             fsw_client_app,
             fsw_client_sup,
             fsw_node_sup,
             fsw_eventlog,
             fsw_eventlog_handler,
             fsw_worker,
             fsw_worker_sup,
	     fsw_master_server,
             visitor_callback
            ]},
  {registered, []},
  {applications, [kernel, stdlib]},
  {env, [
         {master_node, master@rsqlogin01},
         {clients_per_node, "10"},
         {max_files_per_block, "512"},
         {client_sleep_msec, "500"},
         {callback_module, visitor_callback}
        ]},
  {mod, {fsw_client_app, [] }}
 ]}.
