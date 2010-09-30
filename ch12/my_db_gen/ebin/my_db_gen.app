{application, my_db_gen,
 [{description, "Value Key Database"},
  {vsn, "0.1"},
  {modules, [db, my_db_gen, db_sup, db_app]},
  {registered, [db, my_db_gen]},
  {applications, [kernel, stdlib]},
  {mod, {db_app,[]}}]}.
