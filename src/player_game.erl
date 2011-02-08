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

score(Pid,Pins) ->
    Pid ! {self(), {pins, Pins}},
    ok.


%%----------------------------------------------------------------- 
%% Initialisation of process to keep track of player performance
%%----------------------------------------------------------------- 

init(PlayerName) ->
    loop({PlayerName,1,1,false,normal,normal,10,0}).

%%----------------------------------------------------------------- 
%% Loop to continuously receive messages about player performance
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

compute_status(Pins, Frame, Shot, BonusShot, MaxPins, LastShot) ->    
    case Frame of
	% Special treatment required for last (tenth) frame when the player
	% can under specific conditions have 3 shots
	10 -> 
	    case Shot of
		% First shot
		1 ->
		    if
			% Toppled all the pins
			Pins =:= MaxPins ->
			    ThisShotStatus = strike,
			    % Cannot goto next frame since this is the last frame
			    NextFrame = 10,
			    NextShot = 2,
			    NextMaxPins = 10,
			    NextBonusShot = true;
			% Toppled some or zero pins
			true ->
			    ThisShotStatus = normal,
			    NextFrame = 10,
			    NextShot = 2,
			    NextMaxPins = 10 - Pins,
			    NextBonusShot = false
		    end;
		% Second shot
		2 ->
		    if
			% Toppled all the pins
			Pins =:= MaxPins ->
			    case BonusShot of 
				false ->ThisShotStatus = spare;
				% if this is a bonus shot, then no subsequent
				% bonuses can accrue so it should not be marked
				% as a spare
				true -> ThisShotStatus = normal
			    end,
			    % Have to award one more shot in this frame since
			    % player had a spare
			    NextFrame = 10,
			    NextShot = 3,
			    NextMaxPins = 10;
			% Toppled some or zero pins but first shot was a strike
			LastShot =:= strike ->
			    ThisShotStatus = normal,
			    % Have to award one more shot in this frame since
			    % player had a strike earlier
			    NextFrame = 10,
			    NextShot = 3,
			    NextMaxPins = MaxPins - Pins;
			% Toppled some or zero pins and first shot was not a strike
			true ->
			    ThisShotStatus = normal,
			    % GAME OVER
			    NextFrame = none,
			    NextShot = none,
			    NextMaxPins = none
		    end,
		    NextBonusShot = true;
		% Third shot (under scenarios of strike or spare)
		3 ->
		    % the status is irrelevant so do not bother to compute
		    ThisShotStatus = irrelevant,
		    NextFrame = none,
		    NextShot = none,
		    NextMaxPins = none,
		    % This is also actually an irrelevant setting
		    NextBonusShot = true
	    end;
	% For all but the last frame
	_ ->
	    % Bonus shots are applicable only in the last frame .. so always false
	    NextBonusShot = false,
	    case Shot of
		1 ->
		    if 
			Pins =:= MaxPins ->
			    % Strike - toppled all the pins
			    ThisShotStatus = strike,
			    % No more shots in this frame - advance to next
			    NextFrame = Frame + 1,
			    NextShot = 1,
			    NextMaxPins = 10;
			true ->
			    ThisShotStatus = normal,
			    NextFrame = Frame,
			    NextShot = 2,
			    NextMaxPins = 10 - Pins
		    end;
		2 ->
		    NextFrame = Frame + 1,
		    NextShot = 1,
		    % Since the frame is being advanced, pins go back to 10
		    NextMaxPins = 10,
		    ThisShotStatus = 
			case Pins of
			    MaxPins -> spare;
			    _ -> normal
			end
	    end
    end,
    {NextFrame, NextShot, ThisShotStatus, NextMaxPins, NextBonusShot}.
    
loop({PlayerName, Frame, Shot, BonusShot, LastShot, PriorToLastShot, MaxPins, Score}=State) ->
    % The next line is only temporary. Will be removed later
    io:format("Looping. State:~p.~n",[State]),
    receive
	% Handle stop message if received
	{From, stop} ->
            From ! {message, {PlayerName, {status, stopped}, {score, Score}}},
	    ok;
	% Handle score pins message
	{From, {pins, Pins}} when Pins >= 0 andalso Pins =< MaxPins ->
	    Addition = compute_addition(Pins, LastShot, PriorToLastShot,BonusShot),
	    % Compute the addition to the score
            NextScore = Score + Addition,
	    % Compute ShotStatus, NextFrame, NextShot, NextMaxPins
	    {NextFrame, NextShot, ThisShotStatus, NextMaxPins, NextBonusShot} =
		compute_status(Pins, Frame, Shot, BonusShot, MaxPins, LastShot),
	    NextState = {PlayerName,
			 NextFrame,
			 NextShot,
			 NextBonusShot,
			 ThisShotStatus,
			 LastShot,
			 NextMaxPins,
			 NextScore},
	    % Send Message to controller
	    From ! {message, PlayerName, {scored, Pins, Addition}, NextState},
	    loop(NextState);
	{From, {pins, Pins}} ->
	    % this is an invalid input. So just ignore it
	    From ! {message, {ignored_score, Pins}, State},
	    loop(State);
	{From, UnknownMessage} ->
	    From ! {message, {ignored_message, UnknownMessage}, State},
	    loop(State);
	UnknownMessage ->
	    io:format("Unknown message. ignoring. ~n~p~n",[UnknownMessage]),
	    loop(State)
    end.

