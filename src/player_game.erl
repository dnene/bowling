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
    loop(PlayerName,0).

%%----------------------------------------------------------------- 
%% Loop to continuously receive messages about player performance
%%----------------------------------------------------------------- 

loop(PlayerName, Score) ->
    receive
	{From, stop} ->
            From ! {message, {PlayerName, {status, stopped}, {score, Score}}},
	    ok;
	{From, {points, Points}} ->
	    if 
		Points =< 10 ->
		    From ! {message, {PlayerName, {scored, Points}, {score, Score + Points}}},
		    loop(PlayerName,Score + Points);
		true ->
		    From ! {message, {PlayerName, {ignored_score, Points}, {score, Score}}},
		    loop(PlayerName, Score)
	    end
    end.

