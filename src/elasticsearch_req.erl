%%% Created : 09. Mar 2017 2:40 PM
%%% @author johnleytondiaz
%%% @copyright (C) 2017, jdiaz
%%% @doc This module defines all the request functions and interacts
%%% with the cluster
-module(elasticsearch_req).

-include("elasticsearch.hrl").

%% main request functions
-export([direct/4, direct/6, req/1, req/2, req/3, req/4, plain/1]).
%% Misc
-export([plain/2, build_url/3, encode/1, params/2]).


-spec build_url(string(), [string()], [{Key :: string(), Val :: string()}]) -> string().
%%
%% @doc Obtains an URL string formed with a base URL appending a set of segments slash-separated forming a path, and
%% a set of key-value pairs forming an Query string (rfc3986).
%%
build_url(BaseEndpoint, Segments, Values) ->
  BaseEndpointString = string:strip(BaseEndpoint, right, $/),
  SegmentsString =
    case Segments of
      [] -> "";
      _ -> "/" ++ string:join([http_uri:encode(V) || V <- Segments], "/")
    end,
  QueryString =
    case Values of
      [] -> "";
      _ ->
        EncodedValues = [http_uri:encode(K) ++ "=" ++ http_uri:encode(V) || {K, V} <- Values],
        "?" ++ string:join(EncodedValues, "&")
    end,
  BaseEndpointString ++ SegmentsString ++ QueryString.

-spec req(Endpoint :: string() | list()) -> term().
%% @equiv req(Endpoint, get)
req(Endpoint)->
  req(Endpoint, get).

-spec req(Endpoint :: string() | list(), Method :: atom()) -> term().
%% @equiv req(Endpoint, Method, [])
req(Endpoint, Method)->
  req(Endpoint, Method, []).

-spec req(Endpoint :: string() | list(), Method :: atom(), Headers :: list())
      -> term().
%% @equiv req(Endpoint, Method, Headers, [])
req(Endpoint, Method, Headers)->
  req(Endpoint, Method, Headers, []).

-spec req(Endpoint :: string() | list(), Method :: atom(),
    Headers :: list(), Body :: term()
) -> term().
%% @equiv req(Endpoint, Method, Headers, Body, callback())
req(Endpoint, Method, Headers, Body)->
  req(Endpoint, Method, Headers, Body, callback()).

-spec req(Endpoint :: string() | list(), Method :: atom(),
    Headers :: list(), Body :: term(), Success :: term()
) -> term().
%% @equiv req(Endpoint, Method, Headers, Body, Success, callback())
req(Endpoint, Method, Headers, Body, Success)->
  req(Endpoint, Method, Headers, Body, Success, callback()).

-spec req(Endpoint :: string() | list(), Method :: atom(),
    Headers :: list(), Body :: term(), Success :: term(), Error :: term()
) -> term().
%% @doc Performs an http request using ibrowse library, this request does
%% use of gen_server which will fail if response takes longer than OTP gen_server
%% timeout, for long running requests please use direct/6
req(Endpoint, Method, Headers, Body, Success, Error)->
  Opt = elasticsearch_app:get_env(options, []),
  Timeout = elasticsearch_app:get_env(timeout, 30000),
  io:format("URL ~p ~n~n=====~n", [url(Endpoint)]),
  R = ibrowse:send_req(url(Endpoint), Headers, Method, encode(Body), Opt, Timeout),
  response(R, Endpoint, Method, Success, Error).

-spec direct(Endpoint :: string() | list(), Method :: atom(),
    Headers :: list(), Body :: term()
) -> term().
%% @equiv direct(Endpoint, Method, Headers, Body, callback, callback)
direct(Endpoint, Method, Headers, Body)->
  direct(Endpoint, Method, Headers, Body, callback(), callback()).

-spec direct(Endpoint :: string() | list(), Method :: atom(),
    Headers :: list(), Body :: term(), Success :: term(), Error :: term()
) -> term().
%% @doc Performs a direct http request using httpc library
direct(Endpoint, Method, Headers, Body0, Success, Error)->
  HTTPcOptions = elasticsearch_app:get_env(
    httpc_options, ?HTTPC_OPTIONS
  ),
  Body = encode(Body0),
  Request = get_direct_request(url(Endpoint), Method, Body, Headers),
  HTTPOptions = elasticsearch_app:get_env(http_options, ?DEFAULT_HTTP_OPT),
  R = httpc:request(Method, Request, HTTPOptions, HTTPcOptions),
  response(R, Endpoint, Method, Success, Error).

-spec plain(Endpoint :: string() | list()) -> term().
%% @equiv plain(Endpoint, get)
plain(Endpoint)->
  plain(Endpoint, get).

-spec plain(Endpoint :: string() | list(), Method :: atom()) -> term().
%% @equiv req(Endpoint, Method, [], [], text)
plain(Endpoint, Method)->
  req(Endpoint, Method, [], [], text).


-spec params(Endpoint :: string() | list(), Params :: list()) -> list().
%% @doc Returns Endpoint if Params is an empty list, otherwise will
%% build an url query string using the params and will concat with the endpoint
params(Endpoint, [])->
  Endpoint;
params(Endpoint, Params0)->
  Params = string:join(
    [join(K, V) || {K, V} <- Params0],
    "&"
  ),
  endpoint(Endpoint, utils:is_string(Endpoint)) ++ "?" ++ Params.

-spec join(K :: string() | list(), V :: term()) -> list().
%% @private Builds a key=vale pair
join(K, V)->
  string:join([utils:need_list(K), utils:need_list(V)], "=").

-spec url(E :: string() | list()) -> string().
%% @doc Builds an url with the given endpoint and the params got from config
url(E)->
  Endpoint = endpoint(E, utils:is_string(E)),
  Port = utils:need_list(elasticsearch_app:get_env(port, 9200)),
  Host = elasticsearch_app:get_env(host, "localhost") ++ ":" ++ Port,
  protocol() ++ Host ++ "/" ++ Endpoint.

-spec endpoint(E :: string() | list(), boolean()) -> list().
%% @doc Builds an url with the given endpoint
endpoint(Endpoint, true)->
  utils:need_list(Endpoint);
endpoint([], false)->
  "";
endpoint(Endpoint, false)->
  Tokens = [utils:need_list(E) || E <- Endpoint],
  lists:flatten(lists:join("/", Tokens)).

-spec report(E :: string() | list(), term(), Method :: atom()) -> term().
%% @private Builds an url with the given endpoint
report(_, {error, timeout}, head)->
  ok;
report(_, {error, timeout}, _)->
  %%TODO retry
  ok;
report(Endpoint, Error, Method)->
  utils:err("Failed req: method: ~p, endpoint: ~p, error: ~p", [Method, Endpoint, Error]).

-spec callback() -> term().
%% @doc Returns a simple callback function, no side effects
callback()->
  fun(_)-> ok end.

-spec response(R :: term(), Endpoint :: list(), Method :: atom(),
    Success :: term(), Error :: term()) -> term().
%% @private Returns a decoded json string from the http request or an erlang term
response({ok, "200", _Headers, R}, _, _, text, _)->
  R;
response({ok, "200", _Headers, R}, _, _, Success, _)->
  Response = jsx:decode(utils:need_binary(R)),
  Success(Response),
  Response;
response({ok, "404", _, _}, _, _, _, _)->
  not_found;
response({ok, {404, _}}, _, _, _, _)->
  not_found;
response({ok, {200, R}}, _, _, text, _)->
  R;
response({ok, {200, R}}, _, _, Success, _)->
  Response = jsx:decode(utils:need_binary(R)),
  Success(Response),
  Response;
response(Any, Endpoint, Method, _, Error)->
  utils:err("Unexpected response ~p", [Any]),
  Error(Any),
  report(Endpoint, Any, Method),
  [].

-spec protocol() -> list().
%% @doc returns a protocol string to prepend to the url, according to ssl param in config
protocol()->
  protocol(elasticsearch_app:get_env(ssl, false)).

protocol(false)->
  "http://";
protocol(true)->
  "https://".


-spec encode(Json :: term()) -> binary().
%% @doc Returns a json encoded string
encode([])->
  <<>>;
encode(Bin) when is_binary(Bin) ->
  Bin;
encode(Doc) when is_list(Doc); is_tuple(Doc); is_map(Doc) ->
  jsx:encode(Doc).

-spec get_direct_request(Url :: string(), Method :: atom(), Body :: term(),
    Headers :: list()) -> term().
%% @private Builds a tuple with the request parameters to be passed to httpc request
get_direct_request(Url, Method, _, Headers) when Method =/= post, Method =/= put ->
  Length = [{"Content-Length", 0}],
  {Url, lists:ukeysort(1, Headers ++ Length)};
get_direct_request(Url, _, Body, Headers)->
  Length = [{"Content-Length", utils:need_list(erlang:iolist_size(Body))}],
  {Url, lists:ukeysort(1, Headers ++ Length), "application/json", Body}.