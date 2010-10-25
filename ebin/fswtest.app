%% -*- mode: Erlang; fill-column: 75; comment-column: 50; -*-
{application, fswtest,
 [{description, "File System Walker"},
  {vsn, "0.1.0"},
  {modules, [
             fsw_test_app,
             fsw_test,
             fsw_test_sup
            ]},
  {registered, []},
  {applications, [kernel, stdlib]},
  {env, [ {logfile, "fsw.log"},
          {eventfile, "fsw_events.log"},
          {directory, "fsw_events.log"},
          {visitor_callack, visitor_callback} ] },
  {mod, {fsw_test_app, [] }}
  ]}.
