-module(memcache_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%% @private
-spec start(normal, []) -> {ok, pid()}.
start(_StartType, _StartArgs) ->
    memcache_sup:start_link().

%% @private
-spec stop(term()) -> ok.
stop(_State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

