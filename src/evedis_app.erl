-module(evedis_app).

-behaviour(application).

-export([start/2, stop/1]).

%%===================================================================
%% Application callbacks
%%===================================================================
start(_StartType, _StartArgs) ->
    R = evedis:load(),
    evedis_sup:start_link().

stop(_State) ->
    R = evedis:close(),
    ok.
