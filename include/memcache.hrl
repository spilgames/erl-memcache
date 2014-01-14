
-define(MEMCACHE_POOLS_ETS, memcache_pools).
-define(MEMCACHE_DEFAULT_MEMORY_SIZE, 64).

-define(DEBUG(Msg, Args), _ = lager:log(debug, Msg, Args)).
-define(INFO(Msg, Args), _ = lager:log(info, Msg, Args)).
-define(NOTICE(Msg, Args), _ = lager:log(notice, Msg, Args)).
-define(WARNING(Msg, Args), _ = lager:log(warning, Msg, Args)).
-define(ERROR(Msg, Args), _ = lager:log(error, Msg, Args)).

