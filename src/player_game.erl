-module(player_game).
-export([start/1,stop/1,score/2,init/1]).

-author('Dhananjay Nene').


%%----------------------------------------------------------------- 
%% Function to start the game for a particular player
%%----------------------------------------------------------------- 

start(PlayerName) ->
    Pid = spawn(?MODULE, init, [PlayerName]),
    Pid.

%%----------------------------------------------------------------- 
%% API to stop the game for a particular player
%%----------------------------------------------------------------- 

stop(Pid) ->
    Pid ! {self(), stop},
    ok.

%%----------------------------------------------------------------- 
%% API to add score to player game
%%----------------------------------------------------------------- 

score(Pid,Points) ->
    Pid ! {self(), {points, Points}},
    ok.


%%----------------------------------------------------------------- 
%% Initialisation of process to keep track of player performance
%%----------------------------------------------------------------- 

init(PlayerName) ->
    loop(PlayerName,0,{false, false}).

%%----------------------------------------------------------------- 
%% Loop to continuously receive messages about player performance
%%----------------------------------------------------------------- 

loop(PlayerName, Score, {LastStrike, _PriorToLastStrike}=Strike) ->
    % The next line is only temporary. Will be removed later
    io:format("Looping. Player:~p current score:~p, pending: ~p~n",
	      [PlayerName, Score, Strike]),
    receive
	% Handle stop message if received
	{From, stop} ->
            From ! {message, {PlayerName, {status, stopped}, {score, Score}}},
	    ok;

	% Handle score points message
	{From, {points, Points}} ->
	    if 
		% valid points are only between 0 and 10
		Points >= 0 andalso Points =< 10 ->
		    % Addition to the scores depend upon pending strikes also
		    Addition = 
			case Strike of
			    {true, true} -> Points * 3;
                            {true, false} -> Points * 2;
			    {false, true} -> Points * 2;
			    {false, false} -> Points
			end,
                    NextScore = Score + Addition,
		    % Decrement pending for the next loop if it is greater than zero, since
		    % we have accounted for it in Addition above
		    From ! 
			{message, {PlayerName, 
				   {scored, Points}, 
				   {added, Addition},  
				   {score, NextScore},
				   {pending, {Points =:=10, LastStrike}}}},
		    loop(PlayerName,NextScore, {Points =:= 10, LastStrike});
		true ->
		    % this is an invalid input. So just ignore it
		    From ! {message, {PlayerName, {ignored_score, Points}, {score, Score}, {pending, Strike}}},
		    loop(PlayerName, Score, Strike)
	    end
    end.

