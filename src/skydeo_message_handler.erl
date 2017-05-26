-module(skydeo_message_handler).
-behaviour(gen_fsm).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

%% ------------------------------------------------------------------
%% gen_fsm Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3,
         code_change/4]).

%% states
-export([connected/2,connected/3,disconnected/2,disconnected/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_fsm:start_link({local, ?SERVER}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_fsm Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
    {ok, disconnected, []}.

%disconnected state
%async events
disconnected({newconn,Pid}, State) ->
    NewState = [Pid|State],
    io:format("connecting ~p~n",[NewState]),
    {next_state, connected, NewState};

disconnected(_Event, State) ->
    io:format("disconnected~P~n",[_Event,20]),
    {next_state, disconnected, State}.

%sync events
disconnected(_Event, _From, State) ->
    {reply, ok, disconnected, State}.

% connected state
% async events
connected({dropconn,Pid}, State) ->
    State2 = lists:delete(Pid,State),
    ConnState = check_clients(State2),
    {next_state, ConnState, State2};

connected({newconn,Pid}, State) ->
    NewState = [Pid|State],
    {next_state, connected, NewState};

connected({newimage, Img}, State) -> 
    lists:foreach(fun(Pid) -> Pid ! {newimage, Img} end, State),
    {next_state, connected, State};

connected(_Event, State) ->
    io:format("event: ~p~n",[_Event]),
    {next_state, connected, State}.

% synch event
connected(_Event, _From, State) ->
    {reply, ok, connected, State}.

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    {reply, ok, StateName, State}.

handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

check_clients([]) -> 
    disconnected;
check_clients(_) ->
    connected.
