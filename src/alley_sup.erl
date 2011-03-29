-module(alley_sup).
-export([start_link/1]).
-export([init/1]).
-behaviour(supervisor).
-author('Dhananjay Nene').

start_link(LaneCount) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE,[LaneCount]).

init([LaneCount]) ->
    io:format("Here ~w~n",[LaneCount]),
    {ok, {{one_for_one, 1, 10},                       % {RestartStrategy, MaxR, MaxT}
    	  [
	   {game_sup,                                 % id
	    {game_sup, start_link, []},               % StartFunc
	    permanent,                                % Restart
	    10000,                                    % Shutdown
	    supervisor,                               % Type
	    [game]},                                  % Module
	   {alley,                                    % id
	    {alley, start_link, [LaneCount]},         % StartFunc
	    permanent,                                % Restart
	    10000,                                    % Shutdown
	    worker,                                   % Type
	    [alley]}                                  % Module
	  ]
    	 }
    }.
    


