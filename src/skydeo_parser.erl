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
        State

		),
    {reply, ok, State};

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
		%{"resize", "16000@"},
		{"scale", "1x1"},
		{"depth", 8},
		{"colors", 16},
		{"colorspace", "HSB"}
	],
	Out = emagick:convert(Data,jpg,txt,Opts),
	io:format("~P~n",[Out,100]),
	ok. 
