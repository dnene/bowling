-module(game_sup).
-export([start_link/0, start_child/2]).
-export([init/1]).
-behaviour(supervisor).
-author('Dhananjay Nene').

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE,[]).

start_child(Lane, PlayerNames) ->
    supervisor:start_child(?MODULE,[Lane,PlayerNames]).

init([]) ->
    {ok, {{simple_one_for_one, 1, 60},   % {RestartStrategy, MaxR, MaxT}
    	  [{game,                                    % id
    	   {game, start_link, []},                   % StartFunc
    	   permanent,                                % Restart
    	   brutal_kill,                              % Shutdown
    	   worker,                                   % Type
    	   [game]                                    % Module
    	  }]
    	 }
    }.
    


