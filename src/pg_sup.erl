-module(pg_sup).
-export([start_link/0, start_child/1]).
-export([init/1]).
-behaviour(supervisor).
-author('Dhananjay Nene').
-define(SERVER,?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE,[]).

start_child(PlayerName) ->
    supervisor:start_child(?SERVER,[PlayerName]).

init([]) ->
    {ok, {{simple_one_for_one, 1, 60},   % {RestartStrategy, MaxR, MaxT}
    	  [{player_game,                              % id
    	   {player_game, start_link, []},  % StartFunc
    	   temporary,                                % Restart
    	   brutal_kill,                              % Shutdown
    	   worker,                                   % Type
    	   [player_game]                             % Module
    	  }]
    	 }
    }.
    


