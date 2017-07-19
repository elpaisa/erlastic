-ifndef(elasticsearch_hrl).
-define(elasticsearch_hrl, 1).

-define(APPLICATION, elasticsearch).


-define(PROFILE, elasticsearch).
-define(DEFAULT_HTTP_OPT, [{timeout, 5000}]).
-define(HTTPC_OPTIONS, [{body_format, binary}, {full_result, false}]).

-endif.