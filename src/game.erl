-module(game).
-export([start/2, play/3, show_scores/1, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-author('Dhananjay Nene').
-behaviour(gen_server).

%%----------------------------------------------------------------- 
%% Starts a new game. Requires list of player names
%%----------------------------------------------------------------- 

start(Lane, PlayerNames) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE,[Lane, PlayerNames],[]).

stop() ->
    gen_server:cast(?MODULE, stop).

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

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(_Message,State) ->
    {noreply, State}.

%%----------------------------------------------------------------- 
%% Initialisation
%%----------------------------------------------------------------- 
init([Lane, PlayerNames]) ->
    io:format("Into init~n",[]),
    PlayerGames = [{PlayerName, player:create(PlayerName)} ||
		      PlayerName <- PlayerNames],
    io:format("~p~n",[PlayerGames]),
    %% State maintained is
    %%   List of player games in progress but already played, 
    %%   List of PlayerGames in progress but yet to play
    %%   List of All Player Games (including those completed) 
    {ok, {Lane, [],PlayerGames,[]}}.

%%----------------------------------------------------------------- 
%% One turn (ball roll) accounting
%%----------------------------------------------------------------- 
handle_call({play, PlayerName, Pins}, _From, GameState) ->
    case process_score({PlayerName, Pins}, GameState) of 
	{ok, Response, NewState} ->
	    {reply, Response, NewState};
	{game_over, NewState} ->
	    {reply, game_over,NewState};
	_ -> {reply, {error, "Unknown error"}, GameState}
    end;
%%----------------------------------------------------------------- 
%% Get name,score in descending order of scores
%%----------------------------------------------------------------- 
handle_call({show_scores}, _From, {_,Over,Pending,GOver}=State) ->
    {reply,
     lists:sort(fun({_,S1}, {_,S2})-> S1 >= S2 end,
		[{Name, Score} || 
		    {Name, _, _, Score} <- 
			[player:get_summary(X) || 
			    {_,X} <- lists:append([Over,Pending,GOver])]]),
    State}.

    
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
%% Typical most frequent score processing function
%%----------------------------------------------------------------- 
process_score({PlayerName,Pins}, 
	      {Lane,
	       TurnOver,
	       [{PlayerName,PlayerState}|PendingPlayers]=AllPendingPlayers,
	       GameOver}) ->
    io:format("State is ~p~n",[PlayerState]),
    case player:play(PlayerName, Pins, PlayerState) of 
	{turn_over, Response, NewPlayerState} ->
	    % Turn over for current player
	    {{_,NextPlayerState}, NextState} = rearrange_state(Lane,
			     [{PlayerName,NewPlayerState}|TurnOver], 
			     PendingPlayers, 
			     GameOver),
	    {ok, {Response,player:get_summary(NextPlayerState)}, NextState};
	{turn_continue, Response, NewPlayerState} ->
	    % Turn continues for the current player
	    {ok, {Response,player:get_summary(NewPlayerState)},
	     {Lane,TurnOver,[{PlayerName,NewPlayerState}|PendingPlayers],GameOver}};
	{game_over, Response, NewPlayerState} ->
	    % Game over for the current player
	    % TODO : detect is the total game over?
	    case rearrange_state(Lane,
				 TurnOver, 
				 PendingPlayers, 
				 [{PlayerName,NewPlayerState}|GameOver]) of
		{{_,NextPlayerState}, NextState} ->
		    {ok, {Response, player:get_summary(NextPlayerState)}, NextState};
		{none, NextState} ->
		    {ok, {Response, none}, NextState}
	    end;
	{error, Reason} ->
	    {error, {reason, Reason},
	     {Lane,TurnOver,AllPendingPlayers,GameOver}};
	_ ->
	    {error, "unknown error", 
	     {Lane, TurnOver,AllPendingPlayers,GameOver}}
    end.

%%----------------------------------------------------------------- 
%% Rearranging state is required primarily when the Pending list
%% becomes empty, in which case it needs to be recreated from
%% the TurnOver list, just so that the first element from the 
%% Pending list can be extracted and its summary information 
%% returned back to the caller indicating the next player who is
%% required to take is turn and his current game state.
%%----------------------------------------------------------------- 

rearrange_state(Lane, [],[],GameOver) ->
    {none, {Lane,[],[],GameOver}};
rearrange_state(Lane, TurnOver, [], GameOver) ->
    NewPending = lists:reverse(TurnOver),
    [Next|_] = NewPending,
    {Next, {Lane,[], NewPending,GameOver}};
rearrange_state(Lane, TurnOver, [Next|_]=Pending, GameOver) ->
    {Next, {Lane, TurnOver,Pending,GameOver}}.

