-module(fluxer).

-export([init/0]).
-export([init/1]).
-export([set_db/2]).

-export([write_series/2]).
-export([query/2]).

-export([get_databases/1]).
-export([create_database/2]).
-export([delete_database/2]).

-export([get_cluster_admins/1]).
-export([add_cluster_admin/2]).
-export([update_cluster_admin_password/2]).
-export([delete_cluster_admin/2]).
-export([remove_server_from_cluster/2]).

-export([add_database_user/2]).
-export([delete_database_user/2]).
-export([update_user_password/2]).
-export([get_database_users/1]).
-export([add_database_admin_priv/2]).
-export([delete_database_admin_priv/2]).
-export([update_database_user/2]).

-record(flux, {db       :: binary(),
               host     :: binary(),
               port     :: non_neg_integer(),
               user     :: binary(),
               password :: binary(),
               ssl      :: boolean()
              }).

-type ok_result() :: {ok, map()}.
-type error_result() :: {error, integer(), binary()} |
                        {error, db_not_set} |
                        {error, database_already_exists} |
                        {error, influxdb_unavailable}.

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

set_db(Name, Flux) ->
    Flux#flux{ db = Name }.

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
            {error, StatusCode, RespBody};
        {error, econnrefused} ->
            {error, influxdb_unavailable}
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
            {error, StatusCode, RespBody};
        {error, econnrefused} ->
            {error, influxdb_unavailable}
    end.

-spec get_databases(#flux{}) -> ok_result() | error_result().
get_databases(Flux) ->
    case hackney:get(make_url(<<"/db">>, Flux)) of
        {ok, 200, _Headers, ClientRef} ->
            {ok, RespBody} = hackney:body(ClientRef),
            {ok, json:from_binary(RespBody)};
        {ok, StatusCode, _Headers, ClientRef} ->
            {ok, RespBody} = hackney:body(ClientRef),
            {error, StatusCode, RespBody};
        {error, econnrefused} ->
            {error, influxdb_unavailable}
    end.

-spec create_database(binary(), #flux{}) -> ok | error_result().
create_database(Name, Flux) when is_binary(Name) ->
    Data = json:to_binary(#{ name => Name }),
    case hackney:post(make_url(<<"/db">>, Flux), [], Data) of
        {ok, 201, _Headers, _ClientRef} ->
            ok;
        {ok, 409, _Headers, _ClientRef} ->
            {error, database_already_exists};
        {ok, StatusCode, _Headers, ClientRef} ->
            {ok, RespBody} = hackney:body(ClientRef),
            {error, StatusCode, RespBody};
        {error, econnrefused} ->
            {error, influxdb_unavailable}
    end.

-spec delete_database(binary(), #flux{}) -> ok | error_result().
delete_database(Name, Flux) ->
    case hackney:delete(make_url(<<"/db/", Name/binary>>, Flux)) of
        {ok, 200, _Headers, _ClientRef} ->
            ok;
        {ok, StatusCode, _Headers, ClientRef} ->
            {ok, RespBody} = hackney:body(ClientRef),
            {error, StatusCode, RespBody};
        {error, econnrefused} ->
            {error, influxdb_unavailable}
    end.

-spec get_cluster_admins(#flux{}) -> ok_result() | error_result().
get_cluster_admins(Flux) ->
    case hackney:get(make_url(<<"/cluster_admins">>, Flux)) of
        {ok, 200, _Headers, ClientRef} ->
            {ok, RespBody} = hackney:body(ClientRef),
            {ok, json:from_binary(RespBody)};
        {ok, StatusCode, _Headers, ClientRef} ->
            {ok, RespBody} = hackney:body(ClientRef),
            {error, StatusCode, RespBody};
        {error, econnrefused} ->
            {error, influxdb_unavailable}
    end.

-spec add_cluster_admin(map(), #flux{}) -> ok | error_result().
add_cluster_admin(Admin, Flux) when is_map(Admin) ->
    case hackney:post(make_url(<<"cluster_admins">>, Flux), [], json:to_binary(Admin)) of
        {ok, 200, _Headers, _ClientRef} ->
            ok;
        {ok, StatusCode, _Headers, ClientRef} ->
            {ok, RespBody} = hackney:body(ClientRef),
            {error, StatusCode, RespBody};
        {error, econnrefused} ->
            {error, influxdb_unavailable}
    end.

-spec update_cluster_admin_password(map(), #flux{}) -> ok | error_result().
update_cluster_admin_password(#{ name := Name, password := Password }, Flux) ->
    URL = [<<"/cluster_admins/", Name/binary>>],
    case hackney:post(make_url(URL, Flux), [], json:to_binary(#{ password => Password })) of
        {ok, 200, _Headers, _ClientRef} ->
            ok;
        {ok, StatusCode, _Headers, ClientRef} ->
            {ok, RespBody} = hackney:body(ClientRef),
            {error, StatusCode, RespBody};
        {error, econnrefused} ->
            {error, influxdb_unavailable}
    end.

-spec delete_cluster_admin(binary(), #flux{}) -> ok | error_result().
delete_cluster_admin(Name, Flux) when is_binary(Name) ->
    URL = [<<"/cluster_admins/", Name/binary>>],
    case hackney:delete(make_url(URL, Flux)) of
        {ok, 200, _Headers, _ClientRef} ->
            ok;
        {ok, StatusCode, _Headers, ClientRef} ->
            {ok, RespBody} = hackney:body(ClientRef),
            {error, StatusCode, RespBody};
        {error, econnrefused} ->
            {error, influxdb_unavailable}
    end.

-spec add_database_user(map(), #flux{}) -> ok | error_result().
add_database_user(_User, Flux) when Flux#flux.db == undefined ->
    {error, db_not_set};
add_database_user(User, Flux) when is_map(User) ->
    URL = <<"/db/", (Flux#flux.db)/binary, "/users">>,
    case hackney:post(make_url(URL, Flux), [], json:to_binary(User)) of
        {ok, 200, _Headers, _ClientRef} ->
            ok;
        {ok, StatusCode, _Headers, ClientRef} ->
            {ok, RespBody} = hackney:body(ClientRef),
            {error, StatusCode, RespBody};
        {error, econnrefused} ->
            {error, influxdb_unavailable}
    end.

-spec delete_database_user(binary(), #flux{}) -> ok | error_result().
delete_database_user(_Name, Flux) when Flux#flux.db == undefined ->
    {error, db_not_set};
delete_database_user(Name, Flux) when is_binary(Name) ->
    URL = <<"/db/", (Flux#flux.db)/binary, "/users/", Name/binary>>,
    case hackney:delete(make_url(URL, Flux)) of
        {ok, 200, _Headers, _ClientRef} ->
            ok;
        {ok, StatusCode, _Headers, ClientRef} ->
            {ok, RespBody} = hackney:body(ClientRef),
            {error, StatusCode, RespBody};
        {error, econnrefused} ->
            {error, influxdb_unavailable}
    end.

-spec update_user_password(binary(), #flux{}) -> ok | error_result().
update_user_password(_Name, Flux) when Flux#flux.db == undefined ->
    {error, db_not_set};
update_user_password(#{ name := Name, password := Password }, Flux) ->
    URL = <<"/db/", (Flux#flux.db)/binary, "/users/", Name/binary>>,
    case hackney:post(make_url(URL, Flux), [], json:to_binary(#{ password => Password })) of
        {ok, 200, _Headers, _ClientRef} ->
            ok;
        {ok, StatusCode, _Headers, ClientRef} ->
            {ok, RespBody} = hackney:body(ClientRef),
            {error, StatusCode, RespBody};
        {error, econnrefused} ->
            {error, influxdb_unavailable}
    end.

-spec get_database_users(#flux{}) -> ok_result() | error_result().
get_database_users(Flux) when Flux#flux.db == undefined ->
    {error, db_not_set};
get_database_users(Flux) ->
    URL = <<"/db/", (Flux#flux.db)/binary, "/users">>,
    case hackney:get(make_url(URL, Flux)) of
        {ok, 200, _Headers, ClientRef} ->
            {ok, RespBody} = hackney:body(ClientRef),
            {ok, json:from_binary(RespBody)};
        {ok, StatusCode, _Headers, ClientRef} ->
            {ok, RespBody} = hackney:body(ClientRef),
            {error, StatusCode, RespBody};
        {error, econnrefused} ->
            {error, influxdb_unavailable}
    end.

-spec add_database_admin_priv(binary(), #flux{}) -> ok | error_result().
add_database_admin_priv(_Name, Flux) when Flux#flux.db == undefined ->
    {error, db_not_set};
add_database_admin_priv(Name, Flux) ->
    URL = <<"/db/", (Flux#flux.db)/binary, "/users/", Name/binary>>,
    case hackney:post(make_url(URL, Flux), [], json:to_binary(#{ admin => true })) of
        {ok, 200, _Headers, _ClientRef} ->
            ok;
        {ok, StatusCode, _Headers, ClientRef} ->
            {ok, RespBody} = hackney:body(ClientRef),
            {error, StatusCode, RespBody};
        {error, econnrefused} ->
            {error, influxdb_unavailable}
    end.

-spec delete_database_admin_priv(binary(), #flux{}) -> ok | error_result().
delete_database_admin_priv(_Name, Flux) when Flux#flux.db == undefined ->
    {error, db_not_set};
delete_database_admin_priv(Name, Flux) ->
    URL = <<"/db/", (Flux#flux.db)/binary, "/users/", Name/binary>>,
    case hackney:post(make_url(URL, Flux), [], json:to_binary(#{ admin => false })) of
        {ok, 200, _Headers, _ClientRef} ->
            ok;
        {ok, StatusCode, _Headers, ClientRef} ->
            {ok, RespBody} = hackney:body(ClientRef),
            {error, StatusCode, RespBody};
        {error, econnrefused} ->
            {error, influxdb_unavailable}
    end.

-spec update_database_user(map(), #flux{}) -> ok | error_result().
update_database_user(_User, Flux) when Flux#flux.db == undefined ->
    {error, db_not_set};
update_database_user(#{ name := Name } = User, Flux) ->
    URL = <<"/db/", (Flux#flux.db)/binary, "/users/", Name/binary>>,
    case hackney:post(make_url(URL, Flux), [], json:to_binary(maps:without([name], User))) of
        {ok, 200, _Headers, _ClientRef} ->
            ok;
        {ok, StatusCode, _Headers, ClientRef} ->
            {ok, RespBody} = hackney:body(ClientRef),
            {error, StatusCode, RespBody};
        {error, econnrefused} ->
            {error, influxdb_unavailable}
    end.

-spec remove_server_from_cluster(binary(), #flux{}) -> ok | error_result().
remove_server_from_cluster(Server, Flux) ->
    URL = <<"/cluster/servers/", Server/binary>>,
    case hackney:delete(make_url(URL, Flux)) of
        {ok, 200, _Headers, _ClientRef} ->
            ok;
        {ok, StatusCode, _Headers, ClientRef} ->
            {ok, RespBody} = hackney:body(ClientRef),
            {error, StatusCode, RespBody};
        {error, econnrefused} ->
            {error, influxdb_unavailable}
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
