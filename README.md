# fluxer - Erlang client for InfluxDB HTTP API #

Copyright (c) 2014 Sergey Abramyan

__Version:__ 0.1.0

# fluxer

*fluxer* is an client for InfluxDB HTTP API

## Getting Started

*Init default settings*

```erlang
1> Flux = fluxer:init()
```

*Write series*

```erlang
1> Flux = fluxer:init(test_db).
2> Data = #{ name => <<"test_series">>, columns => [<<"col1">>, <<"col2">>], points => [[1, 2]] }.
3> fluxer:write_series(Flux, Data).
ok
```

## TODO

- implement more API
- rewrite init function
- add tests


[![Build Status](https://travis-ci.org/PibbleTeam/fluxer.svg?branch=master)](http://travis-ci.org/PibbleTeam/fluxer)
