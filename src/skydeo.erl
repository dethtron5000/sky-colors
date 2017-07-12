-module(skydeo).

-export([ensure_started/1, send_all/0, parse/2, start/0, timerinternal/3]).
-export([start_timer/1]).


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

start_timer(Duration) ->
	Lst = ["newyork", "cambridge", "paloalto", "chicago", "sanfrancisco", "tokyo", "london", "shanghai", "munich"],
	lists:foreach(fun(Elem) -> timerinternal(Elem,10,Duration) end, Lst).

timerinternal(_,0,_) ->
	ok;

timerinternal(Location, Iteration,Duration) ->
	FileBase = "priv/samples/",
	FilePrefix = "/seq",
	FileEnd = ".jpg",
	File = FileBase ++ Location ++ FilePrefix ++ integer_to_list(Iteration) ++ FileEnd,
	gen_server:cast(skydeo_parser,{parse, File, Location}),
	timer:apply_after(Duration,skydeo,timerinternal,[Location,(Iteration -1),Duration]).



