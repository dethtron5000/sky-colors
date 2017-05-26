-module(skydeo).

-export([ensure_started/1,parse/1]).

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


parse(File) ->
	ensure_deps_started(skydeo),
	gen_server:call(skydeo_parser,{parse,File},20000).


