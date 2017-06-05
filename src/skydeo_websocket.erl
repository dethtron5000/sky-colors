-module(skydeo_websocket).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).
-export([terminate/3]).

init(Req, Opts) ->
	process_flag(trap_exit, true),
	{cowboy_websocket, Req, Opts}.

websocket_init(State) ->
	erlang:start_timer(5000, self(), ping),
	gen_fsm:send_event(skydeo_message_handler,{newconn,self()}),
	{ok, State, hibernate}.

websocket_handle({text, Msg}, State) ->
	Outmsg = {[{<<"msg">>,<< "That's what she said! ", Msg/binary >>}]},
	{reply, {text, jiffy:encode(Outmsg)}, State, hibernate};
websocket_handle(pong, State) ->
	{ok, State, hibernate};
websocket_handle(_Data, State) ->
	io:format("unhandled message: ~p~n",[_Data]),
	{ok, State, hibernate}.

% pings
websocket_info({timeout, _Ref, ping}, State) ->
	erlang:start_timer(5000, self(), ping),
	{reply, ping, State, hibernate};

% new images
websocket_info({newimage, Img}, State) ->
	L = [X || {_,X} <- Img],
	Out = format_message(newimage, L),
	{reply, {text, jiffy:encode(Out)}, State, hibernate};

websocket_info({timeout, _Ref, Msg}, State) ->
	erlang:start_timer(10000, self(), jiffy:encode({[{<<"msg">>,<<"How' you doin'?">>}]})),
	{reply, {text, Msg}, State, hibernate};

websocket_info(_Info, State) ->
	io:format("receiving info: ~p~n",[_Info]),
	{ok, State, hibernate}.

% catch terminations
terminate(Reason, undefined, State) -> 
	io:format("termination: ~p~n~p~n",[Reason,State]),
	ok.

format_message(Type,Contents) ->
	{[{type,Type},{message,Contents}]}.
