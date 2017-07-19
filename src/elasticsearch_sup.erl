%%% Created : 09. Mar 2017 2:40 PM
%%% @author johnleytondiaz
%%% @copyright (C) 2017, jdiaz
%%% @doc elasticsearch top level supervisor.
%%% @end

-module(elasticsearch_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link(Args) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [Args]).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([{Host, Port, Options}]) ->
    Server =
        {elasticsearch_srv,
            {elasticsearch_srv, start_link, [{Host, Port, Options}]},
            permanent, 5000, worker,
            dynamic % XXX
        },
    {ok, { {one_for_all, 0, 1}, [Server]} }.

%%====================================================================
%% Internal functions
%%====================================================================