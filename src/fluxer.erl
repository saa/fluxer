-module(fluxer).

-export([init/0]).
-export([init/1]).
-export([init/2]).
-export([init/3]).
-export([init/5]).

-export([get_databases/1]).
-export([write_series/2]).
-export([query/2]).

-record(flux, {db       = undefined       :: undefined | atom(),
               host     = <<"localhost">> :: binary(),
               port     = 8086            :: non_neg_integer(),
               user     = <<"root">>      :: binary(),
               password = <<"root">>      :: binary(),
               ssl      = false           :: boolean()
              }).

-type ok_result() :: {ok, list()}.
-type error_result() :: {error, integer(), binary()}.
-type db() :: atom().
-type host() :: binary().
-type inf_port() :: non_neg_integer().

%%%===================================================================
%%% API
%%%===================================================================

-spec init() -> #flux{}.
init() ->
    #flux{}.

-spec init(db() | host()) -> #flux{}.
init(Db) when is_atom(Db) ->
    #flux{db = atom_to_binary(Db, utf8)};
init(Host) when is_binary(Host) ->
    #flux{host = Host}.

-spec init(db() | host(), host() | inf_port()) -> #flux{}.
init(Db, Host) when is_atom(Db) ->
    #flux{db = Db, host = Host};
init(Host, Port) when is_binary(Host) ->
    #flux{host = Host, port = Port}.

-spec init(atom(), host(), inf_port()) -> #flux{}.
init(Db, Host, Port) ->
    #flux{db = Db, host = Host, port = Port}.

-spec init(atom(), host(), inf_port(), binary(), binary()) -> #flux{}.
init(Db, Host, Port, User, Password) ->
    #flux{db = Db, host = Host, port = Port, user = User, password = Password}.

-spec get_databases(#flux{}) -> ok_result() | error_result().
get_databases(Flux) ->
    case hackney:get(make_url(<<"/db">>, Flux)) of
        {ok, 200, _Headers, ClientRef} ->
            {ok, RespBody} = hackney:body(ClientRef),
            {ok, jsxn:decode(RespBody)};
        {ok, StatusCode, _Headers, ClientRef} ->
            {ok, RespBody} = hackney:body(ClientRef),
            {error, StatusCode, RespBody}
    end.

-spec write_series(#flux{}, map()) -> ok | error_result().
write_series(Flux, Data) ->
    case hackney:post(make_series_url(Flux), [], jsx:encode([Data])) of
        {ok, 200, _Headers, _ClientRef} ->
            ok;
        {ok, StatusCode, _Headers, ClientRef} ->
            {ok, RespBody} = hackney:body(ClientRef),
            {error, StatusCode, RespBody}
    end.

-spec query(#flux{}, binary()) -> ok_result() | error_result().
query(Flux, Query) when is_binary(Query) ->
    case hackney:get(make_series_url(Flux, [{<<"q">>, Query}])) of
        {ok, 200, _Header, ClientRef} ->
            {ok, RespBody} = hackney:body(ClientRef),
            {ok, jsx:decode(RespBody)};
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
