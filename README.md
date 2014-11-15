# fluxer - Erlang client for InfluxDB #

Copyright (c) 2014 Sergey Abramyan

__Version:__ 0.1.0

[![Build Status](https://travis-ci.org/PibbleTeam/fluxer.svg?branch=master)](http://travis-ci.org/PibbleTeam/fluxer)

## Getting Started

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

## API

**Get databases**

```erlang

1> Test = fluxer:init().
{flux,undefined,<<"127.0.0.1">>,8086,<<"root">>,<<"root">>,false}
2> fluxer:get_databases(Test).
{ok,[#{<<"name">> => <<"test">>}]}
```

## TODO

- implement more API
- add tests
