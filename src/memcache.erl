%% @doc
%% This module acts as entry point for the application.
-module(memcache).

-behaviour(gen_server).

-include("memcache.hrl").

-define(DEFAULT_EXPIRATION_TIME, 3600).

%% API
-export([delete/2,
         flush/1,
         get/2,
         get_pools/0,
         stats/1,
         remove_all_pools/0,
         set/3,
         set/4,
         start/0,
         start_link/0,
         start_pool/6,
         start_pool/7,
         stop/0,
         stop_pool/1
        ]).

-type memcached_opts()::[{memory, non_neg_integer()}].
-type pool_name()::atom().
-type pool_host()::string().
-type pool_port()::pos_integer().
-type pool_size()::pos_integer().
-type pool_max_overflow()::pos_integer().
-type pool()::{pool_name(), pool_host(), pool_port(), pool_size(), pool_max_overflow(), boolean()}.
-type key()::binary()|term().
-type value()::binary()|term().
-type expiration()::pos_integer().
-export_type([pool_name/0, pool_host/0, pool_port/0, pool_size/0, pool_max_overflow/0, key/0,
              value/0, expiration/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state,  {pools::ets:tab()}). %  The ets entries are instances of pool()

%%%===================================================================
%%% API
%%%===================================================================

%% @doc
%% Starts this application
-spec start() -> ok.
%% @end
start() ->
    application:start(?MODULE).

%% @doc
%% Stops this application
-spec stop() -> ok.
%% @end
stop() ->
    application:stop(?MODULE).

%% @private
-spec start_link() -> {ok, pid()}.
%% @end
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Starts a cache pool.
%% The StartServer variable decides whether this application takes care of starting the memcached
%% server as well. If it was already started, just returns ok. Note that the start server
%% functionality will only work for starting local memcached instances, so Host should be a
%% valid name for localhost or an IP address for one of the local network interfaces.
-spec start_pool(pool_name(), pool_host(), pool_port(), memcached_opts(),
                 pool_size(), pool_max_overflow(), boolean()) -> ok | {error, term()}.
%% @end
start_pool(Poolname, Host, Port, MemcachedOpts, Size, MaxOverflow, StartServer) ->
    gen_server:call(?MODULE, {start_pool, {Poolname, Host, Port, MemcachedOpts, Size, MaxOverflow, StartServer}}).

start_pool(Poolname, Host, Port, Size, MaxOverflow, StartServer) ->
    start_pool(Poolname, Host, Port, [], Size, MaxOverflow, StartServer).

%% @doc
%% Stops the given pool. If the memcached server for the pool had been started by this application,
%% it is stopped as well.
%% @end
-spec stop_pool(pool_name()) -> ok | {error, pool_not_found | term()}.
%%
stop_pool(Poolname) ->
    gen_server:call(?MODULE, {stop_pool, Poolname}).

%% @doc
%% retrieve the value with a certain binary key from the specified pool.
%% @end
-spec get(pool_name(), key()) -> {ok, value()} | {error, term()}.
get(Poolname, Key) ->
    Op = fun() ->
            Worker = poolboy:checkout(Poolname),
            case gen_server:call(Worker, {get, Key}) of
                {error, R} ->
                    {error, {poolboy_error, R}};
                Reply ->
                    poolboy:checkin(Poolname, Worker),
                    {ok, Reply}
            end
    end,
    run_in_pool(Poolname, Op).

%%
%% @doc
%% Stores the given value under the given key in the specified pool
%% Uses the default expiration time.
%% @end
-spec set(pool_name(), key(), binary() ) -> {ok, value()} | {error, term()}.
set(Poolname, Key, Value) ->
    set(Poolname, Key, Value, ?DEFAULT_EXPIRATION_TIME).

%% @doc
%% Stores the given value under the given key in the specified pool
%% After the given expiration time (in s), the entry is deleted from the pool.
%% @end
-spec set(pool_name(), key(), value(), expiration() ) -> value().
set(Poolname, Key, Value, Expiration) ->
    Op = fun () ->
            Worker = poolboy:checkout(Poolname),
            case gen_server:call(Worker, {set, Key, Value, Expiration}) of
                {error, R} ->
                    {error, {poolboy_error, R}};
                <<>> ->
                    poolboy:checkin(Poolname, Worker),
                    {ok, Value}
            end
    end,
    run_in_pool(Poolname, Op).

%% @doc
%% Deletes the data associated with a certain key in the specified pool. This functions returns ok
%% even if the key couldn't be found
%% @end
-spec delete(pool_name(), key()) -> ok | {error, term()}.
delete(Poolname, Key) ->
    Op = fun() ->
            Worker = poolboy:checkout(Poolname),
            case gen_server:call(Worker, {delete, Key}) of
                {error, R} ->
                    {error, {poolboy_error, R}};
                _ ->
                    poolboy:checkin(Poolname, Worker),
                    ok
            end
    end,
    run_in_pool(Poolname, Op).

%% @doc
%% Clears all cached entries in the given pool
%% @end
-spec flush(pool_name()) -> ok | {error, term()}.
flush(Poolname) ->
    Op = fun() ->
            Worker = poolboy:checkout(Poolname),
            case gen_server:call(Worker, flush) of
                {error, R} ->
                    {error, {poolboy_error, R}};
                _ ->
                    poolboy:checkin(Poolname, Worker),
                    ok
            end
    end,
    run_in_pool(Poolname, Op).


get_pools() ->
    gen_server:call(?MODULE, get_pools).

-type stats() :: [{atom(), string()}, ...].
%% @doc Pool statistics
-spec stats(pool_name()) -> stats() | {error, term()}.
%% @end
stats(Poolname) ->
    Op = fun() ->
            Worker = poolboy:checkout(Poolname),
            case gen_server:call(Worker, stats) of
                {error, R} ->
                    {error, {poolboy_error, R}};
                Ret ->
                    poolboy:checkin(Poolname, Worker),
                    Ret
            end
    end,
    run_in_pool(Poolname, Op).

%% @doc
%% Removes all the pools started via start_pool/7. This function is automatically called when this
%% application is about to stop.
-spec remove_all_pools() -> [{pool_name(), StopRes::ok | {error, term()}}].
%% @end
remove_all_pools() ->
    gen_server:call(?MODULE, remove_all_pools).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
-spec init([]) -> {ok, #state{}}.
init([]) ->
    ?MEMCACHE_POOLS_ETS = ets:new(?MEMCACHE_POOLS_ETS,
        [public, {read_concurrency, true}, named_table]),
    {ok, #state{pools=?MEMCACHE_POOLS_ETS}}.

%% @private
-type call_type()::{start_pool, pool()} | {stop_pool, pool_name()} |
    remove_all_pools | get_pools | term().
-spec handle_call(call_type(), any(), #state{}) ->
    {reply, ok | {error, term()}, #state{}} | {noreply, #state{}}.
handle_call({start_pool, {Poolname, Host, Port, MemcachedOpts, Size, MaxOverflow, StartServer}}, _From, State) ->
    Res = case check_pool_availability(Poolname) of
        ok ->
            case memcache_pools_sup:add_pool(Poolname, Host, Port, MemcachedOpts,
                                             Size, MaxOverflow, StartServer) of
                {ok, _Pid} ->
                    ok;
                {error, _}=E ->
                    E
            end;
        {error, _}=EE -> EE
    end,
    {reply, Res, State};
handle_call({stop_pool, Poolname}, _From, State) ->
    Res = case ets:lookup(?MEMCACHE_POOLS_ETS, Poolname) of
        [] ->
            {error, pool_not_found};
        [{Poolname, Host, Port, _Size, _MaxOverflow, StopServer}] ->
            case memcache_pools_sup:remove_pool(Poolname, Host, Port, StopServer) of
                ok ->
                    true = ets:delete(?MEMCACHE_POOLS_ETS, Poolname),
                    ok;
                {error, _}=E ->
                    E
            end
    end,
    {reply, Res, State};
handle_call(get_pools, _From, #state{pools=T}=State) ->
    Pools = ets:foldl(fun(X, Acc) -> [element(1, X)|Acc] end, [], T),
    {reply, Pools, State};
handle_call(remove_all_pools, _From, State) ->
    Res = lists:foldl(fun ({Poolname, Host, Port, _Size, _MaxOverflow, StopServer}, Acc) ->
                    PoolRes=memcache_pools_sup:remove_pool(Poolname, Host, Port, StopServer),
                    true = ets:delete(?MEMCACHE_POOLS_ETS, Poolname),
                    [{Poolname, PoolRes}| Acc]
            end, [], ets:tab2list(?MEMCACHE_POOLS_ETS)),
    {reply, Res, State};
handle_call(_Request, _From, State) ->
    {noreply, State}.

%% @private
-spec handle_cast(any(), #state{}) -> {noreply, #state{}}.
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
-spec handle_info(any(), #state{}) -> {noreply, #state{}}.
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
-spec terminate(any(), #state{}) -> ok.
terminate(_Reason, _State) ->
    ok.

%% @private
-spec code_change(any(), #state{}, any()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec run_in_pool(pool_name(), fun (() -> term())) -> term() | {error, pool_not_found | term()}.
run_in_pool(Poolname, Op) ->
    case ets:lookup(?MEMCACHE_POOLS_ETS, Poolname) of
        [] -> {error, pool_not_found};
        [_] ->
            try
                Op()
            catch
                _:Reason ->
                    {error, {poolboy_error, Reason}}
            end
    end.

-spec check_pool_availability(pool_name()) ->
    ok | {error, poolname_in_use}.
check_pool_availability(Poolname) ->
    case ets:lookup(?MEMCACHE_POOLS_ETS, Poolname) of
        [] -> ok;
        [_] -> {error, poolname_in_use}
    end.



