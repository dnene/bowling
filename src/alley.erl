-module(alley).
-export([start_link/1, start_game/1, show_status/0]).
-export([init/1, terminate/2, code_change/3,
	 handle_call/3, handle_cast/2, handle_info/2]).
-author('Dhananjay Nene'). 
-behaviour(gen_server).

%%----------------------------------------------------------------- 
%% Start the alley
%%----------------------------------------------------------------- 

start_link(LaneCount)->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [LaneCount],[]).

%%----------------------------------------------------------------- 
%% Start a new game
%%----------------------------------------------------------------- 

start_game(PlayerNames) ->
    gen_server:call(?MODULE, {start_game, PlayerNames}).

show_status() ->
    gen_server:call(?MODULE, {show_status}).

init([LaneCount]) ->
    {ok, {[lists:concat(["Lane", integer_to_list(X)]) || X <- lists:seq(1,LaneCount)],[]}}.

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
%% Start a new game
%%----------------------------------------------------------------- 

handle_call({start_game, _}, _From, {[],_LanesInUse} = State) ->
    {reply, {error, "No free Lanes"}, State};

handle_call({start_game, PlayerNames}, _From, {[UsableLane|OtherLanes],LanesInUse}) ->
    {ok,Pid} = game_sup:start_child(UsableLane, PlayerNames),
    {reply, {started, Pid, UsableLane}, {OtherLanes,[{UsableLane,PlayerNames,Pid}|LanesInUse]}};

handle_call({show_status}, _From, {_,LanesInUse} = State) ->
    {reply, 
     [{Lane, PlayerName, game:show_scores(Pid)} || {Lane,PlayerName,Pid} <- LanesInUse],
     State}.

