# erlasticsearch

Elasticsearch Erlang Client, OTP 19, rebar3

## Overview

This module allows to perform different operations against a single elasticsearch cluster, 
one of the most interesting features is the `bulk` indexing that allows to push a number 
of documents to the cluster, also there is an `scroll` function that allows to scroll over
a set of results from the cluster, that helps to get a big quantity of documents using an
iterator, and process them as they are retrieved until the result set is done.

If `environment_var` atom is used as host, the module will look in the `os:getenv(ENV_ELASTICSEARCH_HOST)`
for the hostname, if var is not yet defined it will default to `localhost`.

## Configuration
```erlang
%%-*- mode: erlang -*-
[
    {elasticsearch, [
      {host, "localhost"}, %% or environment_var atom
      {port, 9200},
      {ssl, false},
      {max_sessions, 10},
      {shards, 2},
      {replicas, 1},
      {max_pipeline_size, 1},
      {http_options, [{timeout, 1500000}]},
      {basic_auth, {"fake-user", "fake-pass"}}
    ]}
].
% vim:ft=erlang
```

## Ownership

This project is supported by John Diaz, email me for any issues [email][email]

Please see project definition [documentation][api-documentation]

Run:
    make shell
    
    Eshell V8.2  (abort with ^G)
    (erl@local)1> elasticsearch:create_index("index-name").
    (erl@local)1> elasticsearch:index_exists("index-name").

**WARNING:**  This will create and delete a test-index
    Test
    
    Eshell V8.2  (abort with ^G)
    (erl@local)1> elasticsearch:test().
    

Function | Parameters | Description
----- | ----------- | --------
create_index/1 | "index-name" | Checks if the index exists, if not, creates it
create_index/2 | "index-name", {Shards, Replicas}  | Creates an index with settings
path_exists/1 | "path" | checks a specific path in Elasticsearch "index-name/mapping-name/docId"
bulk/2 | {Index, Type}, term() | uses bulk api to push documents into elasticsearch, see [Bulk Api](https://www.elastic.co/guide/en/elasticsearch/reference/2.4/docs-bulk.html)


An erlang client for elasticsearch's REST interface

[email]: mailto:clientes@desarrollowebmedellin.com
[api-documentation]: https://elpaisa.github.io/erlastic/
