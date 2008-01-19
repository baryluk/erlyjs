{application, erlyjs,
 [{description, "ErlyJS Server"},
  {vsn, "0.1"},
  {modules, [
    erlyjs,
    erlyjs_server,
    erlyjs_app,
    erlyjs_sup
  ]},
  {registered, []},
  {applications, [
    kernel,
    stdlib
  ]},
  {included_applications, []},
  {env, []},
  {mod, {erlyjs_app, []}}]}.