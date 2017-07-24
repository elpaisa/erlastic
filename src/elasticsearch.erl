%%% Created : 09. Mar 2017 2:40 PM
%%% @author johnleytondiaz
%%% @copyright (C) 2017, jdiaz
%%% @doc This elasticsearch application implements an HTTP 1.1 client in erlang
%%% This module abstracts the most common operations to perform against an
%%% elasticsearch cluster.
-module(elasticsearch).

-include("elasticsearch.hrl").

-export([ping/0, test/0]).

%% Object operations
-export([create_index/1, create_index/2]).
-export([delete/1, delete/2, mapping/3]).

%% Indexing operations
-export([bulk/1, update/4, update/5]).

%% Search operations
-export([scroll/4, scroll/5, search/3, search/4, count/3, count/4, get/1]).

test() ->
  %%delete("test-index"),
  %%[{<<"acknowledged">>, true}] = create_index("test-index"),
  %%{ok, Bin} = file:read_file("test/test.json"),
  %%[{<<"acknowledged">>, true}] = mapping("test-index", "test_type", Bin),
  Docs = [
    {
      <<"test-index">>, <<"test_type">>, <<"id1">>,
      [
        {<<"record_id">>, 1}, {<<"name">>, <<"None">>}, {<<"customer_id">>, 1},
        {<<"create_date">>, <<"2016-09-14T10:20:09+00:00">>}]
    },
    {
      <<"test-index">>, <<"test_type">>, <<"id2">>,
      [
        {<<"record_id">>, 2}, {<<"name">>, <<"Any">>}, {<<"customer_id">>, 1},
        {<<"create_date">>, <<"2016-09-15T11:20:09+00:00">>}]
    },
    {
      <<"test-index">>, <<"test_type">>, <<"id3">>,
      [
        {<<"record_id">>, 3}, {<<"name">>, <<"Any">>}, {<<"customer_id">>, 1},
        {<<"create_date">>, <<"2016-09-15T12:20:09+00:00">>}]
    }
  ],
  [{<<"took">>,_}, {<<"errors">>, false},_] = bulk(Docs),
  Query = [
    {
      query, [
      {
        bool, [
        {
          must, [{match, [{customer_id, 1}]}]
        },
        {
          filter, [
          {range, [
            {create_date, [
              {gt, <<"2016-09-14T00:00:00+00:00">>},
              {lt, <<"2016-09-16T23:59:59+00:00">>}
            ]}]}
        ]
        }
      ]
      }
    ]
    }
  ],
  io:format("Count ~p ~n", [count("test-index", "test_type", Query)]),
  Fun = fun(Iterator) ->
    utils:inf("Iterator: ~p", [Iterator]),
    ok
        end,
  ok = scroll("test-index", "test_type", Query, Fun, [{size, 1}, {scroll, <<"2m">>}]),
  %%[{<<"acknowledged">>, true}] = delete("test-index"),
  ok.

-spec ping() -> string().
%% @doc Gets the cluster health value
ping() ->
  Health = elasticsearch_req:plain(["_cat", "health"]),
  utils:inf("=CLUSTER HEALTH==== ~p", [Health]),
  Health.

-spec create_index(Name :: list()) -> term().
%% @equiv create_index(Name, {Shards, Replicas})
create_index(Name) ->
  Shards = elasticsearch_app:get_env(shards, 2),
  Replicas = elasticsearch_app:get_env(replicas, 1),
  create_index(Name, {Shards, Replicas}).

-spec create_index(Name :: list(), {Shards :: integer(), Replicas :: integer()}) -> term().
%% @doc Creates an index using the given configuration
%% @param Name name if the index to create
%% @param Shards number fo shards
%% @param Replicas number fo replicas
create_index(Name, {Shards, Replicas}) ->
  create_index(Name, get_index_settings(Shards, Replicas));
create_index(Name, Settings) ->
  do_create_index(path_exists(Name), Name, Settings).


-spec do_create_index(boolean(), Name :: list(), Settings :: list()) -> term().
%% @doc Actual creation stage of the index after validation of existence
%% @param Boolean true|false
%% @param Name name of the index to create
%% @param Settings list of index settings
do_create_index(true, _, _) ->
  {ok, index_exists};
do_create_index(false, Name, Settings) ->
  elasticsearch_req:req(Name, put, [], Settings).

-spec path_exists(Path :: list()) -> term().
%% @doc Gets the given path from elasticsearch
%% @param Path list containing the different pieces from url
path_exists(Path) ->
  do_exist(elasticsearch_req:req(Path, get)).


-spec do_exist(atom()) -> boolean().
%% @doc Used by path_exists to determine true or false according to response
do_exist(not_found) ->
  false;
do_exist(_) ->
  true.

-spec get_index_settings(Shards :: integer(), Replicas :: integer()) -> list().
%% @doc Returns a built query term for an elasticsearch index setting
get_index_settings(Shards, Replicas) ->
  [{settings,
    [{index, [{number_of_shards, Shards}, {number_of_replicas, Replicas}]}]
  }].

-spec mapping(Index :: list(), Type :: list(), Json :: list() | binary()) -> term().
%% @doc Checks if the mapping exists, otherwise creates it
mapping(Index, Type, Json) ->
  elasticsearch_req:req([Index, "_mapping", Type], post, [], Json).

-spec delete(Path :: term()) -> term().
%% @doc Checks if the path exists then deletes it
delete(Path) ->
  delete(Path, path_exists(Path)).

-spec delete(Path :: term(), boolean()) -> term().
%% @doc Issues a delete for the given path, will error if the path doesn't exist
%% to delete safely use delete/1
delete(_, false) ->
  {ok, not_found};
delete(Path, true) ->
  elasticsearch_req:req(Path, delete).

-spec bulk(Bulk :: list()) -> term().
%% @doc Performs a bulk indexing operation against the cluster, this operation
%% performed directly without using the connection pool to avoid gen_server timeouts
%% @param Bulk is [ {Index, Type, Id, Json}, {Index, Type, Id, HeaderInformation, Json}... ]
bulk([])->
  [];
bulk(Bulk) ->
  JsonList = [parse_bulk(B) || B <- Bulk],
  elasticsearch_req:direct("_bulk", post, [], iolist_to_binary(JsonList)).

-spec parse_bulk({Index :: list(), Type :: list(), Id :: term(), Doc :: term()}) -> term().
parse_bulk({Index, Type, Id, Doc}) ->
  parse_bulk({Index, Type, Id, [], Doc});
parse_bulk({Index, Type, Id, HeaderInformation, Doc}) ->
  Header = bulk_index_header(Index, Type, Id, HeaderInformation),
  [Header, <<"\n">>, elasticsearch_req:encode(Doc), <<"\n">>].

-spec bulk_index_header(binary(), binary(), binary(), list()) -> binary().
%% @private Builds a doc digest in elasticsearch bulk format
%% @param Index Index name
%% @param Type is the mapping name
%% @param Id Doc id
%% @param HeaderInformation List of document headers
bulk_index_header(Index, Type, Id, HInf) ->
  IHJson1 = [{<<"_index">>, Index}, {<<"_type">>, Type} | HInf],
  IHJson2 =
    case Id =:= undefined of
      true -> IHJson1;
      false -> [{<<"_id">>, Id} | IHJson1]
    end,
  jsx:encode([{<<"index">>, IHJson2}]).
scroll(Index, Type, Query, Callback)->
  scroll(Index, Type, Query, Callback, []).
scroll(Index, Type, Query, Callback, Params) ->
  E = elasticsearch_req:params([Index, Type, <<"_search">>], Params),
  R = elasticsearch_req:req(E, post, [], Query),
  scroll_results(R, Callback).

scroll_results([{scroll_id, undefined}, _, _, _], _Callback) ->
  false;
scroll_results([{scroll_id, _ScrollId}, {total, Total}, {count, Count}, _], _Callback)
  when Count >= Total ->
  ok;
scroll_results([{scroll_id, ScrollId}, {total, _Total}, {count, Count}, _], Callback) ->
  E = elasticsearch_req:params(["_search", "scroll"], [{scroll, <<"1m">>}, {scroll_id, ScrollId}]),
  Iterator = get_iterator(elasticsearch_req:req(E, get, []), Count),
  Callback(Iterator),
  scroll_results(Iterator, Callback);
scroll_results([], _Callback) ->
  [];
scroll_results(Results, Callback) ->
  Iterator = get_iterator(Results, 0),
  Callback(Iterator),
  scroll_results(Iterator, Callback).

get_iterator(undefined, _CurrentCount) ->
  [
    {scroll_id, undefined},
    {total, 0},
    {count, 0},
    {results, []}
  ];
get_iterator(_Hits, CurrentCount) ->
  Results = proplists:get_value(<<"hits">>, _Hits),
  Hits = proplists:get_value(<<"hits">>, Results),
  ResultsCount = length(Hits) + CurrentCount,
  [
    {scroll_id, proplists:get_value(<<"_scroll_id">>, _Hits)},
    {total, proplists:get_value(<<"total">>, Results)},
    {count, ResultsCount},
    {results, Hits}
  ].

-spec search(Index :: list(), Type :: list(), Query :: list()) -> term().
%% @equiv search(Index, Type, Query, [])
search(Index, Type, Query) ->
  search(Index, Type, Query, []).
-spec search(Index :: list(), Type :: list(), Query :: list(), Params :: list()) -> term().
%% @doc Gets the documents for the given search query
search(Index, Type, Query, Params) ->
  E = elasticsearch_req:params([Index, Type, <<"_search">>], Params),
  elasticsearch_req:req(E, post, [], Query).

-spec count(Index :: list(), Type :: list(), Query :: list()) -> term().
%% @equiv count(Index, Type, Query, [])
count(Index, Type, Query) ->
  count(Index, Type, Query, []).

-spec count(Index :: list(), Type :: list(), Query :: list(), Params :: list()) -> term().
%% @doc Gets the count for the given search query
count(Index, Type, Query, Params) ->
  E = elasticsearch_req:params([Index, Type, <<"_count">>], Params),
  elasticsearch_req:req(E, post, [], Query).

-spec get(Path :: term()) -> term().
%% @doc Issues a get request to the cluster no matter what the path is
%% if the path was not found it will return an empty list "[]"
get(Path)->
  elasticsearch_req:req(Path).

-spec update(Index :: list(), Type :: list(), Id :: list(), Doc :: term()) -> term().
%% @equiv update(Index, Type, Id, Doc, [])
update(Index, Type, Id, Doc) ->
  update(Index, Type, Id, Doc, []).

-spec update(Index :: list(), Type :: list(), Id :: list(), Doc :: term(), Params :: list()
) -> term().
%% @doc Updates a document in the cluster by its id
update(Index, Type, Id, Doc, Params) ->
  E = elasticsearch_req:params([Index, Type, Id, <<"_update">>], Params),
  elasticsearch_req:req(E, post, [], Doc).