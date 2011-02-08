-module(player_game).
-export([start/1,stop/1,init/1]).

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
%% Initialisation of process to keep track of player performance
%%----------------------------------------------------------------- 

init(PlayerName) ->
    loop(PlayerName,0).

%%----------------------------------------------------------------- 
%% Loop to continuously receive messages about player performance
%%----------------------------------------------------------------- 

loop(PlayerName, Score) ->
    receive
	{_From, stop} ->
	    io:format("Thank you ~p for playing. Your current score is ~p. Goodbye!~n",[PlayerName, Score]),
	    ok
    end.

