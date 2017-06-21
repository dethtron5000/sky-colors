-module(skydeo).

-export([ensure_started/1, send_all/0, parse/2, start/0, timerinternal/2]).
-export([start_timer/0]).


ensure_deps_started(App) ->
	application:load(App), 
	{ok, Deps} = application:get_key(App, applications),
	true = lists:all(fun ensure_started/1, Deps),
	ensure_started(App).

ensure_started(App) ->
	case application:start(App) of
		ok ->
			true;
		{error, {already_started, App}} ->
			true;
		Else ->
			error_logger:error_msg("Couldn't start ~p: ~p", [App, Else]),
			Else
	end.

start() ->
	ensure_deps_started(skydeo).

send_all() ->
	ensure_deps_started(skydeo),
	gen_fsm:send_event(skydeo_message_handler, send_all).

parse(File, Location) ->
	ensure_deps_started(skydeo),
	gen_server:call(skydeo_parser,{parse, File, Location},20000).

start_timer() ->
	{ok, _} = timerinternal("newyork",10).

timerinternal(_,0) ->
	ok;

timerinternal(Location, Iteration) ->
	FileBase = "priv/samples/seq",
	FileEnd = ".jpg",
	File = FileBase ++ integer_to_list(Iteration) ++ FileEnd,
	gen_server:call(skydeo_parser,{parse, File, Location},20000),
	timer:apply_after(15000,skydeo,timerinternal,[Location,(Iteration -1)]).



