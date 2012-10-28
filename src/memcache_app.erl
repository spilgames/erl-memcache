-module(memcache_app).

-behaviour(application).

-include_lib("erlanglibs/include/logging.hrl").

%% Application callbacks
-export([start/2, stop/1, prep_stop/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%% @private
-spec start(normal, []) -> {ok, pid()}.
start(_StartType, _StartArgs) ->
    memcache_sup:start_link().

%% @private
-spec stop(any()) -> ok.
stop(_State) ->
    ok.

%% @private
-spec prep_stop(any()) -> any().
prep_stop(_State) ->
    Res = memcache:remove_all_pools(),
    ?INFO("Stopping pools before finishing the application: ~p", [Res]),
    ok.

