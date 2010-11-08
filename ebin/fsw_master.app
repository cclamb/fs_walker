%% -*- mode: Erlang; fill-column: 75; comment-column: 50; -*-
{application, fsw_master,
 [{description, "File System Walker Master Wrapper"},
  {vsn, "0.1.0"},
  {modules, [
             fsw_master_app,
             fsw_master_sup,
             fsw_master_server
            ]},
  {registered, [fsw_master_server]},
  {applications, [kernel, stdlib]},
  {env, [ 
          {nodelist, []},
          {master_node_is_client, false}
        ]},
  {mod, {fsw_master_app, [] }}
 ]}.
