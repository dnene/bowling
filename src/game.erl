-module(game).
-export([start/2, play/3, show_scores/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-author('Dhananjay Nene').
-behaviour(gen_server).

%%----------------------------------------------------------------- 
%% Starts a new game. Requires list of player names
%%----------------------------------------------------------------- 

start(Lane, PlayerNames) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE,[Lane, PlayerNames],[]).

play(Pid, PlayerName, Pins) ->
    gen_server:call(Pid, {play, PlayerName, Pins}).

show_scores(Pid) ->
    gen_server:call(Pid, {show_scores}).

%%----------------------------------------------------------------- 
%% Standard boilerplate callbacks
%%----------------------------------------------------------------- 

code_change(_OldVersion, State, _Extra) ->
     {ok, State}.

terminate(_Reason, _State) ->
    ok.

handle_info(_Message,State) ->
    {noreply, State}.

handle_cast(_Message,State) ->
    {noreply, State}.

%%----------------------------------------------------------------- 
%% Initialisation
%%----------------------------------------------------------------- 
init([Lane, PlayerNames]) ->
    io:format("Into init~n",[]),
    PlayerGames = [{PlayerName, player_game:start(PlayerName)} ||
		      PlayerName <- PlayerNames],
    io:format("~p~n",[PlayerGames]),
    %% State maintained is
    %%   List of player games in progress but already played, 
    %%   List of PlayerGames in progress but yet to play
    %%   List of All Player Games (including those completed) 
    {ok, {Lane, [],PlayerGames,[]}}.

handle_call({play, PlayerName, Pins}, _From, State) ->
    case process_score({PlayerName, Pins}, State) of 
	{ok, Response, NewState} ->
	    {reply, Response, NewState};
	{game_over, State} ->
	    {reply, game_over,State};
	_ -> {reply, {error, "Unknown error"}, State}
    end;
handle_call({show_scores}, _From, {_,Over,Pending,GOver}) ->
    [{Name, Score} || {Name, _, _, Score} <- [player_game:get_summary(X) || X <- lists:append(Over,Pending,GOver)]].

    
%%----------------------------------------------------------------- 
%% No games in the completed this turn and none in waiting for a turn
%% which means game is over
%%----------------------------------------------------------------- 
process_score(_,{_,[],[],_GameOver}=State) ->
    {game_over,State};

%%----------------------------------------------------------------- 
%% All games have completed this turn. So just move them all into
%% the waiting for next turn (switch the game to move to next turn)
%%----------------------------------------------------------------- 
process_score({PlayerName,Pins}, {Lane,TurnOver,[],GameOver}) -> 
    process_score({PlayerName,Pins}, {Lane,[],lists:reverse(TurnOver),GameOver});

%%----------------------------------------------------------------- 
%%----------------------------------------------------------------- 
process_score({PlayerName,Pins}, {Lane,TurnOver,[PlayerState|PendingPlayers]=AllPendingPlayers,GameOver}) ->
    case player_game:play(PlayerName, Pins, PlayerState) of 
	{turn_over, Response, NextState} ->
	    % Turn over for current player
	    {ok,
	     Response,
	     {Lane,[NextState|TurnOver], PendingPlayers, GameOver}};
	{turn_continue, Response, NextState} ->
	    % Turn continues for the current player
	    {ok, Response,
	     {Lane,TurnOver,[NextState|PendingPlayers],GameOver}};
	{game_over, Response, NextState} ->
	    % Game over for the current player
	    % TODO : detect is the total game over?
	    {ok, Response,
	     {Lane,TurnOver, PendingPlayers, [NextState|GameOver]}};
	{error, Reason} ->
	    {error, {reason, Reason},
	     {Lane,TurnOver,AllPendingPlayers,GameOver}};
	_ ->
	    {error, "unknown error", 
	     {Lane, TurnOver,AllPendingPlayers,GameOver}}
    end.

