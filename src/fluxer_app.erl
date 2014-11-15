-module(fluxer_app).

-behaviour(application).

%% Application callbacks
-export([start/0]).
-export([start/2]).
-export([stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() ->
    application:ensure_all_started(fluxer).

start(_StartType, _StartArgs) ->
    fluxer:init(),
    fluxer_sup:start_link().

stop(_State) ->
    ok.
