%%% Created : 09. Mar 2017 2:40 PM
%%% @doc Elasticsearch client, OTP gen_server module
%%% @author johnleytondiaz
%%% @copyright (C) 2017, jdiaz
-module(elasticsearch_srv).
-author("johnleytondiaz").
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-include("elasticsearch.hrl").

-ifdef(TEST).
-compile([export_all]).
-endif.


-export([start_link/1]).

%% server functions
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3, stop/0]).

%% Sync functions
-export([start/0]).

-record(state, {}).
%%==============================================================================
%% API functions
%%==============================================================================

start_link(Args) ->
  %%observer:start(),
  gen_server:start_link({local, ?SERVER}, ?MODULE, _Args = [Args], []).

%%==============================================================================
%% gen_server callbacks
%%==============================================================================

%% @private
init([{Host, Port, Options}]) ->
  utils:inf("Initializing elasticsearch client ~p", [?MODULE]),
  MaxSessions = proplists:get_value(max_sessions, Options, 10),
  MaxPipelineSize = proplists:get_value(max_pipeline_size, Options, 1),
  ibrowse:set_dest(Host, Port, [{max_sessions, MaxSessions}, {max_pipeline_size, MaxPipelineSize}]),
  elasticsearch:ping(),
  {ok, #state{}}.


handle_call(_Request, _From, State) ->
  {noreply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

start() ->
  ok.
stop() ->
  gen_server:cast(?MODULE, {stop, all}).
