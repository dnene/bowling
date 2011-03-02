%% -*- mode: Erlang; fill-column: 75; comment-column: 50; -*-

{application, tcp_rpc,
 [{description, "Bowling Alley Management System"},
  {vsn, "0.1.0"},
  {modules, [bowling_app, game, pg_sup, player_game]},
  {registered, [pg_sup]},
  {applications, [kernel, stdlib]},
  {mod, {bowling_app, []}},
 ]
}.
