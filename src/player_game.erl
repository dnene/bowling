-module(player_game).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/1,stop/1,score/2,get_state/1]).
-behaviour(gen_server).
-author('Dhananjay Nene').

-include("../include/game_state.hrl").

%%----------------------------------------------------------------- 
%% Function to start the game for a particular player
%%----------------------------------------------------------------- 

start_link(PlayerName) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [PlayerName], []).

%%----------------------------------------------------------------- 
%% API to stop the game for a particular player
%%----------------------------------------------------------------- 

stop(Pid) ->
    Pid ! {self(), stop},
    ok.

%%----------------------------------------------------------------- 
%% API to get current status
%%----------------------------------------------------------------- 

get_state(Pid) ->
    gen_server:call(Pid, get_state).

%%----------------------------------------------------------------- 
%% API to add score to player game
%%----------------------------------------------------------------- 

score(Pid,Pins) ->
    gen_server:call(Pid,{pins,Pins}).


%%----------------------------------------------------------------- 
%% Initialisation of process to keep track of player performance
%%----------------------------------------------------------------- 

init(PlayerName) ->
    {ok, #game_state{
	   player_name        = PlayerName,
	   frame              = 1,
	   shot               = 1,
	   bonus_shot         = false,
	   last_shot          = normal,
	   prior_to_last_shot = normal,
	   max_pins           = 10,
           score              =0}}.

%%----------------------------------------------------------------- 
%% Terminate behaviour
%%----------------------------------------------------------------- 

terminate(_Reason, _State) ->
    ok.

%%----------------------------------------------------------------- 
%% Code change behaviour
%%----------------------------------------------------------------- 

code_change(_OldVersion, State, _Extra) ->
     {ok, State}.

%%----------------------------------------------------------------- 
%% Compute the exact addition to the player's score
%%----------------------------------------------------------------- 

compute_addition(Pins, LastShot, PriorToLastShot, BonusShot) ->
    (
      case BonusShot of
	  false -> 1;
	  true -> 0
      end
      +
      case LastShot of       
	  strike -> 1;
	  spare -> 1;
	  _ -> 0
      end 
      +
      case PriorToLastShot of
	  strike -> 1;
	  _ -> 0
      end
    ) * Pins.

%%----------------------------------------------------------------- 
%% Given the particular pins being toppled, compute the next state
%%----------------------------------------------------------------- 

compute_status(State, Pins, NextScore) ->    
    case State#game_state.frame of
	% Special treatment required for last (tenth) frame when the player
	% can under   specific conditions have 3 shots
	10 -> 
	    case State#game_state.shot of
		% First shot
		1 ->
		    if
			% Toppled all the pins
			Pins =:= State#game_state.max_pins ->
			    State#game_state{
			      shot = 2, bonus_shot = true, last_shot = strike, 
			      prior_to_last_shot = State#game_state.last_shot, 
			      max_pins = 10, score = NextScore};
			% Toppled some or zero pins
			true ->
			    State#game_state{
			      shot = 2, last_shot = normal, 
			      prior_to_last_shot = State#game_state.last_shot, 
			      max_pins = 10 - Pins, score = NextScore}
		    end;
		% Second shot
		2 ->
		    if
			% Toppled all the pins
			Pins =:= State#game_state.max_pins ->
			    case State#game_state.bonus_shot of 
				false ->ThisShotStatus = spare;
				% if this is a bonus shot, then no subsequent
				% bonuses can accrue so it should not be marked
				% as a spare
				true -> ThisShotStatus = normal
			    end,
			    % Have to award one more shot in this frame since
			    % player had a spare
			    State#game_state{
			      shot = 3, bonus_shot = true, 
			      last_shot = ThisShotStatus, 
			      prior_to_last_shot = State#game_state.last_shot, 
			      max_pins = 10, score = NextScore};
			% Toppled some or zero pins but first shot was a strike
			State#game_state.last_shot =:= strike ->
			    % Have to award one more shot in this frame since
			    % player had a strike earlier
			    State#game_state{
			      shot = 3, bonus_shot = true, 
			      last_shot = normal, 
			      prior_to_last_shot = State#game_state.last_shot, 
			      max_pins = State#game_state.max_pins - Pins, 
			      score = NextScore};
			% Toppled some or zero pins and first shot was not a strike
			true ->
			    {game_over, 
			     State#game_state{
			       frame = none,
			       shot = none,
			       bonus_shot = none,
			       last_shot = none,
			       prior_to_last_shot = none,
			       max_pins = none,
			       score = NextScore
			      }}
		    end;
		% Third shot (under scenarios of strike or spare)
		3 ->
		    {game_over, 
		     State#game_state{
		       frame = none,
		       shot = none,
		       bonus_shot = none,
		       last_shot = none,
		       prior_to_last_shot = none,
		       max_pins = none,
		       score = NextScore
		      }}
	    end;
	% For all but the last frame
	_ ->
	    % Bonus shots are applicable only in the last frame .. so always false
	    case State#game_state.shot of
		1 ->
		    if 
			Pins =:= State#game_state.max_pins ->
			    % Strike - toppled all the pins
			    % No more shots in this frame - advance to next
			    State#game_state{
			      frame = State#game_state.frame + 1, shot = 1, 
			      last_shot = strike, 
			      prior_to_last_shot = State#game_state.last_shot, 
			      max_pins = 10, 
			      score = NextScore};
			true ->
			    State#game_state{
			      shot = 2, 
			      last_shot = normal, 
			      prior_to_last_shot = State#game_state.last_shot, 
			      max_pins = 10 - Pins, 
			      score = NextScore}
		    end;
		2 ->
		    ThisShotStatus = 
			case Pins =:= State#game_state.max_pins of
			    true -> spare;
			    false -> normal
			end,
		    State#game_state{
		      frame = State#game_state.frame + 1, shot = 1, 
		      last_shot = ThisShotStatus, 
		      prior_to_last_shot = State#game_state.last_shot, 
		      max_pins = 10, 
		      score = NextScore}
	    end
    end.

%%----------------------------------------------------------------- 
%% Handle request to get state
%%----------------------------------------------------------------- 

handle_call({get_state},_From, State) -> {reply, State, State};

%%----------------------------------------------------------------- 
%% Handle the call to score based on pins being toppled
%%----------------------------------------------------------------- 

handle_call({pins, Pins}, _From, State) when Pins >= 0 andalso Pins =< State#game_state.max_pins ->
    Addition = compute_addition(Pins, 
				State#game_state.last_shot,
				State#game_state.prior_to_last_shot,
				State#game_state.bonus_shot),
    % Compute the addition to the score
           NextScore = State#game_state.score + Addition,
    % Compute ShotStatus, NextFrame, NextShot, NextMaxPins
	    Val = compute_status(State, Pins, NextScore),
    case Val of
    	#game_state{} = NextState -> 
    	    % Send Message to controller
    	    Reply = {game_progressed, NextState, NextState#game_state.score};
    	{game_over, NextState}  ->
    	    Reply = {game_over, NextState},
	    io:format("Game over with : ~p~n",[NextState])
    end,
    {reply, Reply, NextState};
handle_call({pins,_Pins},_From,_State) ->
    {error, invalid}.


%%----------------------------------------------------------------- 
%% Handle casts. Currently none anticipated.
%%----------------------------------------------------------------- 

handle_cast(fail, State) ->
    1/0,
    {noreply, State};
handle_cast(Request, State) ->
    io:format("Received a cast ~w~n",[Request]),
    {noreply, State}.

%%----------------------------------------------------------------- 
%% Handle info messages. Currently none planned.
%%----------------------------------------------------------------- 

handle_info(Info, State) ->
    io:format("Received a info ~w~n",[Info]),
    {noreply, State}.
