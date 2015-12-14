-module(fluxer).

-export([start_link/1]).
-export([pool_name/0]).

-export([create_database/1]).
-export([create_database/2]).
-export([show_databases/0]).

-export([write/2, write/3, write/4]).

-export([select/2]).

-define(POOL_NAME, fluxer_pool).

%%====================================================================
%% API
%%====================================================================

start_link([Host, Port, IsSSL, Options]) ->
    fusco:start_link({Host, Port, IsSSL}, Options).

-spec pool_name() -> atom().
pool_name() ->
    ?POOL_NAME.

-spec create_database(string()) -> term().
create_database(Name) ->
    create_database(Name, false).

-spec create_database(string(), boolean()) -> term().
create_database(Name, IfNotExists) ->
    Query = case IfNotExists of
                true -> ["CREATE DATABASE IF NOT EXISTS ", Name];
                false -> ["CREATE DATABASE ", Name]
            end,
    case query(Query) of
        {ok, _Resp} -> ok;
        Error -> Error
    end.

-spec show_databases() -> term().
show_databases() ->
    query("SHOW DATABASES").

-spec select(string(), string()) -> term().
select(DB, Query) ->
    query(DB, Query).

write(DB, Measurement, Value) ->
    write(DB, Measurement, [], Value).

write(DB, Measurement, [], Value) ->
    Line = iolist_to_binary([Measurement, " value=", to_binary(Value)]),
    write(DB, Line);
write(DB, Measurement, Tags, Value) ->
    ComposedTags = compose_tags(Tags),
    Line = iolist_to_binary([Measurement, ",", ComposedTags, " value=", to_binary(Value)]),
    write(DB, Line).

write(DB, Data) when is_list(Data) ->
    write(DB, list_to_binary(Data));
write(DB, Data) ->
    Path = iolist_to_binary(["/write?db=", DB]),
    Fun = fun(W) ->
                  fusco:request(W, Path, "POST", [], Data, 5000)
          end,
    case poolboy:transaction(?POOL_NAME, Fun) of
        {ok, {{<<"204">>, _}, _Hdrs, _Resp, _, _}} -> ok;
        Error -> Error
    end.

%%====================================================================
%% Internal functions
%%====================================================================

-spec query(iodata() | binary()) -> term().
query(Query) when is_list(Query) ->
    query(iolist_to_binary(Query));
query(Query) when is_binary(Query) ->
    query_2(iolist_to_binary(["/query?q=", Query])).

query(DB, Query) ->
    query_2(iolist_to_binary(["/query?db=", DB, "&q=", Query])).

query_2(Query) ->
    Query2 = binary:replace(Query, <<" ">>, <<"%20">>, [global]),
    Fun = fun(W) ->
                  fusco:request(W, Query2, "GET", [], [], 5000)
          end,
    case poolboy:transaction(?POOL_NAME, Fun) of
        {ok, {{<<"200">>, _}, _Hdrs, Resp, _, _}} ->
            {ok, jsx:decode(Resp)};
        Error ->
            Error
    end.

compose_tags(Tags) ->
    compose_tags(Tags, <<>>).

compose_tags([], Acc) ->
    Acc;
compose_tags([{Key, Value} | Rest], <<>>) ->
    NewAcc = <<(to_binary(Key))/binary, "=", (to_binary(Value))/binary>>,
    compose_tags(Rest, NewAcc);
compose_tags([{Key, Value} | Rest], Acc) ->
    NewAcc = <<(to_binary(Key))/binary, "=", (to_binary(Value))/binary, ",", Acc/binary>>,
    compose_tags(Rest, NewAcc).

to_binary(Val) when is_integer(Val) ->
    integer_to_binary(Val);
to_binary(Val) when is_list(Val) ->
    list_to_binary(Val);
to_binary(Val) when is_atom(Val) ->
    atom_to_binary(Val, utf8);
to_binary(Val) when is_float(Val) ->
    list_to_binary(float_to_list(Val)).
