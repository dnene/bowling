-module(game).
-export([start/1, score/1, stop/1]).
-export([init/1]).
-author('Dhananjay Nene').

-include("../include/game_state.hrl").

%%----------------------------------------------------------------- 
%% Starts a new game. Requires list of player names
%%----------------------------------------------------------------- 

start(PlayerNames) ->
    spawn(?MODULE, init, [PlayerNames]).
    

%%----------------------------------------------------------------- 
%% Initialisation
%%----------------------------------------------------------------- 
init(PlayerNames) ->
    io:format("Into init~n",[]),
    PlayerGames = [{PlayerName, player_game:start(PlayerName)} ||
		      PlayerName <- PlayerNames],
    io:format("~p~n",[PlayerGames]),
    loop(PlayerGames,PlayerGames,[]).

%%----------------------------------------------------------------- 
%%----------------------------------------------------------------- 
loop(PlayerGames,[],PlayerPerformances) -> loop(PlayerGames,PlayerGames,PlayerPerformances);
loop([],[],PlayerPerformances) ->
    io:format("Game over. Scores are ~p~n",[PlayerPerformances]);
loop(PlayerGames,[{_Player,Game}|RemainingPlayerGames],PlayerPerformances) ->
    PlayerState = player_game:get_state(Game),
    loop(PlayerGames, Game, PlayerState, RemainingPlayerGames,PlayerPerformances).

loop(PlayerGames, Game, PlayerState, RemainingPlayerGames,PlayerPerformances) ->    
    case io:fread(
	   io_lib:format("Player ~p on ~p to play frame ~p shot ~p. Please enter score: ",
	      [PlayerState#game_state.player_name,
	       PlayerState#game_state.score,
	       PlayerState#game_state.frame,
	       PlayerState#game_state.shot]),
	   "~d"
	  ) of
	{ok, [Value]} ->
	    player_game:score(Game,Value),
	    receive
		{_From, {game_progressed, #game_state{} = NextState,_}} ->
		    io:format("Player ~p score is now ~p~n",
			      [NextState#game_state.player_name,
			       NextState#game_state.score]),
		    case NextState#game_state.shot of
			1 ->
			    % Turn over
			    loop(PlayerGames, RemainingPlayerGames, PlayerPerformances);
			2 ->
			    % Turn continues for the same player
			    loop(PlayerGames, Game, NextState, RemainingPlayerGames,
				 PlayerPerformances);
			3 ->
			    % Turn continues for the same player
			    loop(PlayerGames, Game, NextState, RemainingPlayerGames,
				 PlayerPerformances)
                    end;
		{_From, {game_over,#game_state{} = NextState}} ->
		    io:format("Game over for player ~p with score ~p~n",
			      [NextState#game_state.player_name,
			       NextState#game_state.score]),
		    loop(lists:delete(NextState#game_state.player_name,PlayerGames),
			 RemainingPlayerGames, 
			 [{NextState#game_state.player_name,
			   NextState#game_state.score}|PlayerPerformances]);
		Unexpected ->
		    io:format("Unexpected response: ~p. Aborting.~n",[Unexpected])
            end;
	Unexpected ->
	    io:format("Error in reading console input: ~p~n",[Unexpected]),
	    loop(PlayerGames, Game, PlayerState, RemainingPlayerGames,PlayerPerformances)
    end.

%%----------------------------------------------------------------- 
%%----------------------------------------------------------------- 

score(_) ->
    ok.
%%----------------------------------------------------------------- 
%%----------------------------------------------------------------- 

stop(_) ->
    ok.
%%----------------------------------------------------------------- 
%%----------------------------------------------------------------- 
%%----------------------------------------------------------------- 
%%----------------------------------------------------------------- 
