-module(memcache).

-behaviour(gen_server).

-define(DEFAULT_EXPIRATION_TIME, 3600).
-define(MEMCACHE_POOLS_ETS, memcache_pools).

%% API
-export([delete/2,
         flush/1,
         get/2,
         set/3,
         set/4,
         start/0,
         start_link/0,
         start_pool/5,
         stop/0,
         stop_pool/1
        ]).

-type pool_name()::atom().
-type pool_host()::string().
-type pool_port()::pos_integer().
-type pool_size()::pos_integer().
-type pool_max_overflow()::pos_integer().
-type pool()::{pool_name(), pool_host(), pool_port(), pool_size(), pool_max_overflow()}.
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
%% Starts the application and all the necessary dependencies. It returns a function that stops
%% all that is needed in order to leave the environment as it was.
-spec start() -> {ok, fun (() -> ok)}.
%% @end
start() ->
    elibs_application:start(?MODULE).

%% @doc
%% Stops this application
-spec stop() -> ok.
%% @end
stop() ->
    application:stop(?MODULE).

%% @doc Starts the server
-spec start_link() -> {ok, pid()}.
%% @end
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Starts a cache pool.
%% If it was already started, just returns ok.
-spec start_pool(pool_name(), pool_host(), pool_port(), pool_size(), pool_max_overflow()) ->
    ok | {error, term()}.
%% @end
start_pool(Poolname, Host, Port, Size, MaxOverflow) ->
    gen_server:call(?MODULE, {start_pool, {Poolname, Host, Port, Size, MaxOverflow}}).

%% @doc
%% Stops the given pool
%% @end
-spec stop_pool(pool_name()) -> ok | {error, pool_not_found}.
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
            Reply = gen_server:call(Worker, {get, Key}),
            poolboy:checkin(Poolname, Worker),
            {ok, Reply}
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
            <<>> = gen_server:call(Worker, {set, Key, Value, Expiration}),
            poolboy:checkin(Poolname, Worker),
            {ok, Value}
    end,
    run_in_pool(Poolname, Op).

%% @doc
%% Deletes the data associated with a certain key in the specified pool.
%% @end
-spec delete(pool_name(), key()) -> ok | {error, term()}.
delete(Poolname, Key) ->
    Op = fun() ->
            Worker = poolboy:checkout(Poolname),
            gen_server:call(Worker, {delete, Key}),
            poolboy:checkin(Poolname, Worker),
            ok
    end,
    run_in_pool(Poolname, Op).

%% @doc
%% Clears all cached entries in the given pool
%% @end
-spec flush(pool_name()) -> ok | {error, term()}.
flush(Poolname) ->
    Op = fun() ->
            Worker = poolboy:checkout(Poolname),
            Reply = gen_server:call(Worker, flush ),
            poolboy:checkin(Poolname, Worker),
            Reply
    end,
    run_in_pool(Poolname, Op).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec init([]) -> {ok, #state{}}.
init([]) ->
    ?MEMCACHE_POOLS_ETS=ets:new(?MEMCACHE_POOLS_ETS,
                                    [protected, {read_concurrency, true}, named_table]),
    {ok, #state{pools=?MEMCACHE_POOLS_ETS}}.

-type call_type()::{start_pool, pool()} | {stop_pool, pool_name()} | term().
-spec handle_call(call_type(), any(), #state{}) ->
    {reply, ok | {error, term()}, #state{}} | {noreply, #state{}}.
handle_call({start_pool, {Poolname, Host, Port, Size, MaxOverflow}}, _From, State) ->
    Res = case check_pool_availability(Poolname, Port) of
        ok ->
            case memcache_pools_sup:add_pool(Poolname, Host, Port, Size, MaxOverflow) of
                {ok, _Pid} ->
                    true=ets:insert(?MEMCACHE_POOLS_ETS,
                                    [{Poolname, Host, Port, Size, MaxOverflow}]),
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
            ok;
        [{Poolname, Host, Port, _Size, _MaxOverflow}] ->
            case memcache_pools_sup:remove_pool(Poolname, Host, Port) of
                ok ->
                    true = ets:delete(?MEMCACHE_POOLS_ETS, Poolname),
                    ok;
                {error, _}=E ->
                    E
            end
    end,
    {reply, Res, State};
handle_call(_Request, _From, State) ->
    {noreply, State}.

-spec handle_cast(any(), #state{}) -> {noreply, #state{}}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(any(), #state{}) -> {noreply, #state{}}.
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(any(), #state{}) -> ok.
terminate(_Reason, _State) ->
    ok.

-spec code_change(any(), #state{}, any()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec run_in_pool(pool_name(), fun (() -> term())) -> term() | {error, pool_not_found}.
run_in_pool(Poolname, Op) ->
    case ets:lookup(?MEMCACHE_POOLS_ETS, Poolname) of
        [] -> {error, pool_not_found};
        [_] -> Op()
    end.

-spec check_pool_availability(pool_name(), pool_port()) ->
    ok | {error, poolname_in_use | addr_in_use}.
check_pool_availability(Poolname, Port) ->
    case ets:lookup(?MEMCACHE_POOLS_ETS, Poolname) of
        [] ->
            case gen_tcp:listen(Port, [binary, {reuseaddr, true}]) of
                {ok, Sock} ->
                    gen_tcp:close(Sock),
                    ok;
                {error, Reason} when Reason == eaddrinuse; Reason == econnrefused ->
                    {error, addr_in_use}
            end;
        [_] ->
            {error, poolname_in_use}
    end.

