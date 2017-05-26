-module(skydeo_parser).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% Called by async_function_wrapper
%% ------------------------------------------------------------------

-export([parse_file/2]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------


start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
    {ok, Args}.

handle_call({parse,File}, From, State) ->
	skydeo_util:async_func_wrapper(
        ?MODULE, 
        parse_file, 
        [File], 
        From, 
        State),
    {noreply,State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

parse_file(File,_State) ->
	{ok, Data} = file:read_file(File),
	%io:format("~p~n",[Data]),
	Opts = [
		{"resize", "1600@"},
		%{"scale", "200x200"},
		{"depth", 8},
		{"colors", 16},
		{"colorspace", "RGB"}
	],
	{ok, [Out]} = emagick:convert(Data,jpg,txt,Opts),
	Z = binary:split(Out,<<"\n">>,[global]),
	ColorElems = lists:map(fun extract_colors/1, Z),
	io:format("~P~n",[ColorElems,100]),
	Zout = lists:foldl(fun accumulator/2, orddict:new(), ColorElems),
	io:format("~P~n",[Zout,100]). 

extract_colors(ColorLine) ->
	Colors = [hex,r,g,b],
	% ,([\d]{1,3}),([\d]{1,3})\\)
	Z = re:run(ColorLine, get_pattern(),[{capture,Colors,binary}]),
	io:format("~P~P~n",[ColorLine,100,Z,100]),
	parse_match(Z,Colors).

parse_match({match,ColorList},Colors) ->
	lists:zip(Colors,ColorList);
parse_match(nomatch,_) ->
	[].

get_pattern() ->
	{ok, Pattern} = re:compile("#(?<hex>[0-9A-F]{6})[\s]+rgb\\((?<r>[0-9]{1,3}),(?<g>[0-9]{1,3}),(?<b>[0-9]{1,3})\\)",[unicode]),
	Pattern.

accumulator(Elem, Acc) ->
	Hex = proplists:get_value(hex,Elem),
	Initial = Elem ++ [{count,1}],
	case Hex of
		undefined -> Acc;
		_ -> orddict:update(Hex, fun updater/1, Initial, Acc)
	end.
	
updater(Elem) ->
	Val = proplists:get_value(count,Elem),
	NewList = proplists:delete(count, Elem),
	NewList ++ [{count, Val + 1}].


	

