{application, aliter,
 [{description, "Aliter main server."},
  {vsn, "0"},
  {modules, [aliter]},
  {included_applications, [login, char, zone]},
  {applications, [kernel, stdlib]},
  {mod, {aliter, []}}
 ]}.
