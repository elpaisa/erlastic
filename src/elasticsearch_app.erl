%%% @doc elasticsearch public API
%%% @author johnleytondiaz
%%% @copyright (C) 2017, jdiaz
%%%
%%% @end
%%% Created : 09. Mar 2017 2:40 PM

-module(elasticsearch_app).

-include("elasticsearch.hrl").

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1, get_env/1, get_env/2, set_env/2]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
	Host = get_env(host),
	Port = get_env(port),
	Options = get_env(options),
    elasticsearch_sup:start_link({Host, Port, Options}).

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
-spec get_env(atom()) -> term() | 'undefined'.
%% @doc get an environment variable's value (or undefined if it doesn't exist)
get_env(Key) ->
  get_env(Key, 'undefined').


-spec get_env(atom(), term()) -> term().
%% @doc get an environment variable's value (or Default if it doesn't exist)
get_env(Key, Default) ->
  case application:get_env(?APPLICATION, Key) of
    {ok, X} -> X;
    'undefined' -> Default
  end.


-spec set_env(atom(), any()) -> ok.
%% @doc set the environment variable's value
set_env(Key, Value) ->
  application:set_env(?APPLICATION, Key, Value).
