-module(skydeo_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
	Parser = ?CHILD(skydeo_parser,worker),
	Webserver = ?CHILD(skydeo_webserver,worker),
	MessageHandler = ?CHILD(skydeo_message_handler,worker),
	Storage = ?CHILD(skydeo_storage,worker),

    {ok, { {one_for_one, 5, 10}, [Parser, Webserver, MessageHandler, Storage]} }.

