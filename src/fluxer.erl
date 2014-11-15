-module(fluxer).

-export([init/0]).
-export([init/1]).
-export([get_databases/1]).
-export([write_series/2]).
-export([query/2]).

-record(flux, {db       :: binary(),
               host     :: binary(),
               port     :: non_neg_integer(),
               user     :: binary(),
               password :: binary(),
               ssl      :: boolean()
              }).

-type ok_result() :: {ok, map()}.
-type error_result() :: {error, integer(), binary()} | {error, db_not_set}.

%%%===================================================================
%%% API
%%%===================================================================

-spec init() -> #flux{}.
init() ->
    init(#{}).

-spec init(map()) -> #flux{}.
init(Config) ->
    #flux{
         db = maps:get(db, Config, undefined),
         host = maps:get(host, Config, <<"127.0.0.1">>),
         port = maps:get(port, Config, 8086),
         user = maps:get(user, Config, <<"root">>),
         password = maps:get(password, Config, <<"root">>),
         ssl = maps:get(ssl, Config, false)
        }.

-spec get_databases(#flux{}) -> ok_result() | error_result().
get_databases(Flux) ->
    case hackney:get(make_url(<<"/db">>, Flux)) of
        {ok, 200, _Headers, ClientRef} ->
            {ok, RespBody} = hackney:body(ClientRef),
            {ok, json:from_binary(RespBody)};
        {ok, StatusCode, _Headers, ClientRef} ->
            {ok, RespBody} = hackney:body(ClientRef),
            {error, StatusCode, RespBody}
    end.

-spec write_series(#flux{}, map()) -> ok | error_result().
write_series(Flux, _Data) when Flux#flux.db == undefined ->
    {error, db_not_set};
write_series(Flux, Data) ->
    Data2 = lists:flatten([Data]),
    case hackney:post(make_series_url(Flux), [], json:to_binary(Data2)) of
        {ok, 200, _Headers, _ClientRef} ->
            ok;
        {ok, StatusCode, _Headers, ClientRef} ->
            {ok, RespBody} = hackney:body(ClientRef),
            {error, StatusCode, RespBody}
    end.

-spec query(#flux{}, binary()) -> ok_result() | error_result().
query(Flux, _Query) when Flux#flux.db == undefined ->
    {error, db_not_set};
query(Flux, Query) when is_binary(Query) ->
    case hackney:get(make_series_url(Flux, [{<<"q">>, Query}])) of
        {ok, 200, _Header, ClientRef} ->
            {ok, RespBody} = hackney:body(ClientRef),
            {ok, json:from_binary(RespBody)};
        {ok, StatusCode, _Headers, ClientRef} ->
            {ok, RespBody} = hackney:body(ClientRef),
            {error, StatusCode, RespBody}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec make_series_url(#flux{}) -> binary().
make_series_url(Flux) ->
    make_series_url(Flux, []).

-spec make_series_url(#flux{}, list()) -> binary().
make_series_url(Flux, Qs) ->
    Path = iolist_to_binary([<<"/db/">>, Flux#flux.db, <<"/series">>]),
    make_url(Path, Flux, Qs).

-spec make_url(binary() | list(), #flux{}) -> binary().
make_url(Path, Flux) when is_list(Path) ->
    make_url(iolist_to_binary(Path), Flux);
make_url(Path, Flux) ->
    make_url(Path, Flux, []).

-spec make_url(binary() | list(), #flux{}, list()) -> binary().
make_url(Path, Flux, Qs) when is_list(Path) ->
    make_url(iolist_to_binary(Path), Flux, Qs);
make_url(Path, Flux, Qs) ->
    Qs2 = [{<<"u">>, Flux#flux.user}, {<<"p">>, Flux#flux.password} | Qs],
    Protocol = case Flux#flux.ssl of
                   false -> <<"http://">>;
                   true -> <<"https://">>
               end,
    URI = iolist_to_binary([Protocol, Flux#flux.host, ":", integer_to_list(Flux#flux.port)]),
    hackney_url:make_url(URI, <<$/, Path/binary>>, Qs2).
