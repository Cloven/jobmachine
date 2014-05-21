-module(jobmachine_app).
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

-spec start(_, _) -> { ok, pid()}.
start(_Type, _Args) ->
    case ets:info(ws_subscriptions) of
      undefined -> ets:new(ws_subscriptions, [bag, named_table, public]);
      _ -> ok
    end,
    jobmachine_sup:start_link().

-spec stop(_S) -> ok.
stop(_State) ->
    ok.
