-module(player_tests).
-include_lib("eunit/include/eunit.hrl").

play_results(PlayerName, Pins, StateBefore, Result) ->
    ?assertEqual(
       player:play(PlayerName, Pins, StateBefore),
       Result).

initial_simple_play_test() ->
    play_results("foo",0, 
		 {player_state, "foo", 1, 1, false, normal, normal, 10, 0},
		 {turn_continue, 
		  {0,0,2,10}, 
		  {player_state, "foo", 1, 2, false, normal, normal, 10, 0}}).

invalid_player_test() ->
    play_results("bar",0, 
		 {player_state, "foo", 1, 1, false, normal, normal, 10, 0},
		 {error, io_lib:format("It is player ~w's turn to play. ~n",["foo"])}).

strike_test() ->
    play_results("foo",10, 
		 {player_state, "foo", 1, 1, false, normal, normal, 10, 0},
		 {turn_over, 
		  {10,10,1,10}, 
		  {player_state, "foo", 2, 1, false, strike, normal, 10, 10}}).

spare_test() ->
    play_results("foo",5, 
		 {player_state, "foo", 1, 2, false, normal, normal, 5, 5},
		 {turn_over, 
		  {5,10,1,10}, 
		  {player_state, "foo", 2, 1, false, spare, normal, 10, 10}}).

