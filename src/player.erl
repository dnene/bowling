-module(player).
-export([create/1, play/3, get_summary/1]).
-author('Dhananjay Nene').

%%-------------------------------------------------------------
%% Record that contains the current game state for a player
%%-------------------------------------------------------------
-record(player_state, 
	{
	  player_name,
	  frame,
	  shot,
	  bonus_shot,
	  last_shot,
	  prior_to_last_shot,
	  max_pins,
	  score
	}).


%%----------------------------------------------------------------- 
%% Initialisation of process to keep track of player performance
%%----------------------------------------------------------------- 

create(PlayerName) ->
    #player_state{
	   player_name        = PlayerName,
	   frame              = 1,
	   shot               = 1,
	   bonus_shot         = false,
	   last_shot          = normal,
	   prior_to_last_shot = normal,
	   max_pins           = 10,
           score              =0}.

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
    case State#player_state.frame of
	% Special treatment required for last (tenth) frame when the player
	% can under   specific conditions have 3 shots
	10 -> 
	    case State#player_state.shot of
		% First shot
		1 ->
		    if
			% Toppled all the pins
			Pins =:= State#player_state.max_pins ->
			    {turn_continue, State#player_state{
			      shot = 2, bonus_shot = true, last_shot = strike, 
			      prior_to_last_shot = State#player_state.last_shot, 
			      max_pins = 10, score = NextScore}};
			% Toppled some or zero pins
			true ->
			    {turn_continue, State#player_state{
			      shot = 2, last_shot = normal, 
			      prior_to_last_shot = State#player_state.last_shot, 
			      max_pins = 10 - Pins, score = NextScore}}
		    end;
		% Second shot
		2 ->
		    if
			% Toppled all the pins
			Pins =:= State#player_state.max_pins ->
			    case State#player_state.bonus_shot of 
				false ->ThisShotStatus = spare;
				% if this is a bonus shot, then no subsequent
				% bonuses can accrue so it should not be marked
				% as a spare
				true -> ThisShotStatus = normal
			    end,
			    % Have to award one more shot in this frame since
			    % player had a spare
			    {turn_continue, State#player_state{
			      shot = 3, bonus_shot = true, 
			      last_shot = ThisShotStatus, 
			      prior_to_last_shot = State#player_state.last_shot, 
			      max_pins = 10, score = NextScore}};
			% Toppled some or zero pins but first shot was a strike
			State#player_state.last_shot =:= strike ->
			    % Have to award one more shot in this frame since
			    % player had a strike earlier
			    {turn_continue, State#player_state{
			      shot = 3, bonus_shot = true, 
			      last_shot = normal, 
			      prior_to_last_shot = State#player_state.last_shot, 
			      max_pins = State#player_state.max_pins - Pins, 
			      score = NextScore}};
			% Toppled some or zero pins and first shot was not a strike
			true ->
			    {game_over, 
			     State#player_state{
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
		     State#player_state{
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
	    case State#player_state.shot of
		1 ->
		    if 
			Pins =:= State#player_state.max_pins ->
			    % Strike - toppled all the pins
			    % No more shots in this frame - advance to next
			    {turn_over, State#player_state{
			      frame = State#player_state.frame + 1, shot = 1, 
			      last_shot = strike, 
			      prior_to_last_shot = State#player_state.last_shot, 
			      max_pins = 10, 
			      score = NextScore}};
			true ->
			    {turn_continue, State#player_state{
			      shot = 2, 
			      last_shot = normal, 
			      prior_to_last_shot = State#player_state.last_shot, 
			      max_pins = 10 - Pins, 
			      score = NextScore}}
		    end;
		2 ->
		    ThisShotStatus = 
			case Pins =:= State#player_state.max_pins of
			    true -> spare;
			    false -> normal
			end,
		    {turn_over, State#player_state{
		      frame = State#player_state.frame + 1, shot = 1, 
		      last_shot = ThisShotStatus, 
		      prior_to_last_shot = State#player_state.last_shot, 
		      max_pins = 10, 
		      score = NextScore}}
	    end
    end.

%%----------------------------------------------------------------- 
%% Handle the call to score based on pins being toppled
%%----------------------------------------------------------------- 

play(PlayerName, Pins, State) 
  when PlayerName == State#player_state.player_name 
       andalso Pins >= 0
       andalso Pins =< State#player_state.max_pins ->
    Addition = compute_addition(Pins, 
				State#player_state.last_shot,
				State#player_state.prior_to_last_shot,
				State#player_state.bonus_shot),
    % Compute the addition to the score
    NextScore = State#player_state.score + Addition,
    % Compute ShotStatus, NextFrame, NextShot, NextMaxPins
    case compute_status(State, Pins, NextScore) of 
	{PlayStatus,NextState} ->
	    {PlayStatus,{Addition,
			 NextScore,
			 NextState#player_state.frame,
			 NextState#player_state.shot,
			 NextState#player_state.max_pins},
	     NextState};
	_ -> {error, "Unknown error"}
    end;

play(PlayerName, _, State) when PlayerName =/= State#player_state.player_name ->
    {error,  
     io_lib:format("It is player ~w's turn to play. ~n",
		   [State#player_state.player_name])};

play(_, _, State) ->
    {error,  
     io_lib:format("Pins must be less than or equal to ~p",
		   [State#player_state.max_pins])}.

%%-------------------------------------------------------------
%% helper getter methods
%%-------------------------------------------------------------

get_summary(PlayerState) ->
    {PlayerState#player_state.player_name,
     PlayerState#player_state.frame,
     PlayerState#player_state.shot,
     PlayerState#player_state.score}.
