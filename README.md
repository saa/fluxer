# fluxer - Erlang client for InfluxDB #

Copyright (c) 2014 Sergey Abramyan

__Version:__ 0.2.0

[![Build Status](https://travis-ci.org/PibbleTeam/fluxer.svg?branch=master)](http://travis-ci.org/PibbleTeam/fluxer)

## API

**Results**

*ok_result:*

- ok
- {ok, map()}

*error_result:*

- {error, integer(), binary()}
- {error, db_not_set}
- {error, database_already_exists}
- {error, influxdb_unavailable}

**Init flux object**

Init with default settings

```erlang

1> Flux = fluxer:init()
{flux,undefined,<<"127.0.0.1">>,8086,<<"root">>,<<"root">>,false}
```

If need other settings, call fluxer:init/1. Example:

```erlang

1> Config = #{
    db => <<"test">>,
    port => 8086,
    user => <<"myuser">>,
    password => <<"mypassword">>,
    ssl => true }.
2> Flux = fluxer:init(Config).
{flux,<<"test">>,<<"localhost">>,8086,<<"myuser">>,<<"mypassword">>,true}
```

### Reading & Writing Data

**Init default settings with database**

```erlang

1> Test = fluxer:init(#{ db => <<"test">> }).
{flux,<<"test">>,<<"127.0.0.1">>,8086,<<"root">>,<<"root">>,false}
```

**Write series**

```erlang

1> Test = fluxer:init(#{ db => <<"test">> }).
{flux,<<"test">>,<<"127.0.0.1">>,8086,<<"root">>,<<"root">>,false}
2> Data = #{ name => test_series, columns => [col1, col2], points => [[1, 2]] }.
#{columns => [col1,col2],name => test_series,points => [[1,2]]}
3> fluxer:write_series(Flux, Data).
ok
```

**Query**

```erlang

1> Test = fluxer:init(#{ db => <<"test">> }).
{flux,<<"test">>,<<"127.0.0.1">>,8086,<<"root">>,<<"root">>,false}
2> fluxer:query(Test, <<"select * from test_series">>).
{ok,[#{<<"columns">> => [<<"time">>,<<"sequence_number">>,<<"col2">>,
        <<"col1">>],
       <<"name">> => <<"test_series">>,
       <<"points">> => [[1416060930055,290001,2,1]]}]}
```

### Administration & Security

#### Databases

**Get databases**

```erlang

1> Test = fluxer:init().
{flux,undefined,<<"127.0.0.1">>,8086,<<"root">>,<<"root">>,false}
2> fluxer:get_databases(Test).
{ok,[#{<<"name">> => <<"test">>}]}
```

**Create database**

```erlang

1> fluxer:create_database(<<"db1">>, Flux).
ok
```

**Delete database**

```erlang

1> fluxer:delete_database(<<"db1">>, Flux).
ok
```

#### Cluster

**Get cluster admins**

```erlang

1> fluxer:get_cluster_admins(Flux).
{ok,[#{<<"name">> => <<"root">>}]}
```

**Add cluster admin**

```erlang

1> fluxer:add_cluster_admin(#{ name => <<"admin">>, password => <<"super password">> }, Flux).
ok
```

**Update password for cluster admin**

```erlang

1> fluxer:update_cluster_admin_password(#{ name => <<"admin">>, password => <<"new password">> }, Flux).
ok
```

**Delete cluster admin**

```erlang

1> fluxer:delete_cluster_admin(<<"admin">>, Flux).
ok
```

**Remove server from cluster**

```erlang

1> fluxer:remove_server_from_cluster(<<"34">>, Flux).
ok
```

#### Security

**Add database user**

```erlang

1> Flux = fluxer:init(#{ db => <<"test">> }).
2> fluxer:add_database_user(#{ name => <<"user1">>, password => <<"password1">> }, Flux).
ok
```

**Delete database user**

```erlang

1> Flux = fluxer:init(#{ db => <<"test">> }).
2> fluxer:delete_database_user(<<"user1">>, Flux).
ok
```

**Update user password**

```erlang

1> Flux = fluxer:init(#{ db => <<"test">> }).
2> fluxer:update_user_password(#{ name => <<"user1">>, password => <<"new password1">> }, Flux).
ok
```

**Get database users**

```erlang

1> Flux = fluxer:init(#{ db => <<"test">> }).
2> fluxer:get_database_users(Flux).
{ok,[#{<<"isAdmin">> => true,
       <<"name">> => <<"test">>,
       <<"readFrom">> => <<".*">>,
       <<"writeTo">> => <<".*">>},
     #{<<"isAdmin">> => false,
       <<"name">> => <<"user1">>,
       <<"readFrom">> => <<".*">>,
       <<"writeTo">> => <<".*">>},
     #{<<"isAdmin">> => false,
       <<"name">> => <<"user2">>,
       <<"readFrom">> => <<".*">>,
       <<"writeTo">> => <<".*">>}]}
```

**Add databse user admin privileges**

```erlang

1> Flux = fluxer:init(#{ db => <<"test">> }).
2> fluxer:add_database_admin_priv(<<"user1">>, Flux).
ok
```

**Delete databse user admin privileges**

```erlang

1> Flux = fluxer:init(#{ db => <<"test">> }).
2> fluxer:delete_database_admin_priv(<<"user1">>, Flux).
ok
``

**Limiting user access**

```erlang

1> Flux = fluxer:init(#{ db => <<"test">> }).
2> fluxer:update_database_user(#{ name => <<"user1">>, readFrom => <<"^$">>, writeTo => <<".*">> }, Flux).
ok
```

## TODO

- implement more API
- add tests
