%%-------------------------------------------------------------
%% Record that contains the current game state for a player
%%-------------------------------------------------------------
-record(game_state, 
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
