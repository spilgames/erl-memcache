%% @doc
%% Supervises the pools and memcached instances managed by the application, restarting them in case
%% of failure with a one for one strategy
-module(memcache_pools_sup).

-behaviour(supervisor).

-include("memcache.hrl").

%% API
-export([add_pool/6,
         add_pool/7,
         remove_pool/4,
         start_link/0,
         start_pool/3
]).

%% Supervisor callbacks
-export([init/1]).

%% just for testing purposes
-ifdef('TEST').
-export([start_memcache/3, stop_memcache/2, wait_for_memcache/4]).
-endif.

-define(CHECK_PORT_RETRIES, 5).
-define(PORT_RETRY_INTERVAL, 200).
-define(CHILD(Id, Mod, Type, Args), {Id, {Mod, start_link, Args},
                                     permanent, 5000, Type, [Mod]}).

%%%===================================================================
%%% API functions
%%%===================================================================

%% @doc
%% Starts this supervisor. To be called from memcache_sup
-spec start_link() -> {ok, pid()}.
%% @end
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


%% @doc
%% Adds a pool to the list of children of this supervisor
-spec add_pool(memcache:pool_name(), memcache:pool_host(), memcache:pool_port(),
               memcache:pool_size(), memcache:pool_max_overflow(), StartServer::boolean()) ->
    {ok, pid()} | {error, term()}.
%% @end
add_pool(Poolname, Host, Port, Size, MaxOverflow, StartServer) ->
    add_pool(Poolname, Host, Port, [], Size, MaxOverflow, StartServer).

%% @doc
%% Adds a pool to the list of children of this supervisor
-spec add_pool(memcache:pool_name(), memcache:pool_host(), memcache:pool_port(), memcache:memcached_opts(),
               memcache:pool_size(), memcache:pool_max_overflow(), StartServer::boolean()) ->
    {ok, pid()} | {error, term()}.
%% @end
add_pool(Poolname, Host, Port, MemcachedOpts, Size, MaxOverflow, StartServer) ->
    ChildSpec = build_child_spec(Poolname, Host, Port, MemcachedOpts, Size, MaxOverflow, StartServer),
    supervisor:start_child(?MODULE, ChildSpec).

%% @doc
%% Terminates a pool and removes its childspec from this supervisor
-spec remove_pool(memcache:pool_name(), memcache:pool_host(), memcache:pool_port(),
                  ServerManaged::boolean()) -> ok | {error, term()}.
%% @end
remove_pool(Poolname, Host, Port, true) ->
    ok=stop_memcache(Host, Port),
    remove_pool(Poolname, Host, Port, false);
remove_pool(Poolname, _Host, _Port, false) ->
    poolboy:stop(Poolname),
    case supervisor:terminate_child(?MODULE, Poolname) of
        ok ->
            ok = supervisor:delete_child(?MODULE, Poolname);
        {error, not_found} ->
            {error, not_found}
    end.

%% @private
%% @doc
%% This functions wraps the poolboy call to start the pool of connections with the start_memcache/3
%% call in order to start the server before if needed.
%%
%% If all is fine, registers the pool to ?MEMCACHE_POOL_ETS
-spec start_pool({start | already_started, memcache:pool_host(), memcache:pool_port(), memcache:memcached_opts()}
    | undefined, [{atom(), term()}], tuple()) -> {ok, pid()} | {error, term()}.
%% @end
start_pool({start, Host, Port, MemcachedOpts}, PoolboyOpts, CreateOpts) when is_list(Host), is_integer(Port) ->
    ?INFO("Starting memcache pool and memcached server at ~p", [{Host, Port}]),
    start_memcache(Host, Port, MemcachedOpts),
    case do_start_pool(Host, Port, PoolboyOpts) of
        {ok, Pid} ->
            true = ets:insert(?MEMCACHE_POOLS_ETS, [CreateOpts]),
            {ok, Pid};
        {error, _} = Err ->
            Err
    end;
start_pool({already_started, Host, Port, _}, PoolboyOpts, CreateOpts) when is_list(Host), is_integer(Port) ->
    ?INFO("Starting memcache pool", []),
    case do_start_pool(Host, Port, PoolboyOpts) of
        {ok, Pid} ->
            true = ets:insert(?MEMCACHE_POOLS_ETS, [CreateOpts]),
            {ok, Pid};
        {error, _} = Err ->
            Err
    end.

%%%===================================================================
%%% Supervisor Callbacks
%%%===================================================================

%% @private
init([]) ->
    Children = lists:map(
        fun ({Poolname, Props}) ->
                Size = proplists:get_value(size, Props),
                MaxOverflow = proplists:get_value(max_overflow, Props),
                Port = proplists:get_value(port, Props),
                Host = proplists:get_value(host, Props),
                MemcachedOpts = proplists:get_value(opts, Props, []),
                Start = proplists:get_value(start_server, Props),
                build_child_spec(Poolname, Host, Port, MemcachedOpts, Size, MaxOverflow, Start)
        end,
        get_config_value(pools, [])
    ),
    {ok, {{one_for_one, 5, 10}, Children}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec build_child_spec(memcache:pool_name(), memcache:pool_host(), memcache:pool_port(),
                        memcache:pool_config(), memcache:pool_size(), memcache:pool_max_overflow(), boolean()) ->
    supervisor:child_spec().
build_child_spec(Poolname, Host, Port, MemcachedOpts, Size, MaxOverflow, StartServer) ->
    StartServerOpts=case StartServer of
        true -> {start, Host, Port, MemcachedOpts};
        false -> {already_started, Host, Port, MemcachedOpts}
    end,
    CreateOpts = {Poolname, Host, Port, Size, MaxOverflow, StartServer},
    PoolboyOpts = [{name, {local, Poolname}}, {worker_module, erlmc_conn}, {size, Size},
                   {max_overflow, MaxOverflow}, {host, Host}, {port, Port}],
    {Poolname, {?MODULE, start_pool, [StartServerOpts, PoolboyOpts, CreateOpts]},
     transient, 5000, worker, [poolboy]}.

-spec do_start_pool(memcache:pool_host(), memcache:pool_port(), [{atom(), term()}]) ->
    {ok, pid()} | {error, term()}.
do_start_pool(Host, Port, PoolboyOpts) ->
    case wait_for_memcache(Host, Port, ?CHECK_PORT_RETRIES, ?PORT_RETRY_INTERVAL) of
        ok -> start_poolboy(PoolboyOpts);
        {error, _}=Error -> Error
    end.

-spec start_poolboy([{atom(), term()}]) -> {ok, pid()} | {error, term()}.
start_poolboy(PoolboyOpts) ->
    try
        poolboy:start_link(PoolboyOpts)
    catch
        E:EE ->
            ?WARNING("Unable to start pool: ~p", [{E, EE}]),
            {error, EE}
    end.

-spec wait_for_memcache(memcache:pool_host(), memcache:pool_port(),
                        non_neg_integer(), pos_integer()) -> ok | {error, term()}.
wait_for_memcache(Host, Port, 0, _) ->
    {error, {unable_to_connect_to_memcached, Host, Port}};
wait_for_memcache(Host, Port, Retries, RetryInterval) ->
    case gen_tcp:connect(Host, Port, [binary, {reuseaddr, true}]) of
        {ok, Sock} ->
            gen_tcp:close(Sock),
            ok;
        {error, econnrefused} ->
            timer:sleep(RetryInterval),
            wait_for_memcache(Host, Port, Retries - 1, RetryInterval);
        {error, Reason} ->
            {error, {unable_to_connect_to_memcached, Reason}}
    end.

-spec start_memcache(memcache:pool_host(), memcache:pool_port(), memcache:pool_config()) -> ok | {error, term()}.
start_memcache(Host, Port, MemcachedOpts) ->
    Memory = proplists:get_value(memory, MemcachedOpts, ?MEMCACHE_DEFAULT_MEMORY_SIZE),
    Cmd = lists:flatten(io_lib:format(
              get_config_value(start_memcached, default_command(start_memcached)),
              [Host, Memory, Port]
          )),
    case os:cmd(Cmd) of
        [] ->
            timer:sleep(100),
            ok;
        Other ->
            ?WARNING("Unexpected return starting memcached ~p: ~p", [{Host, Port}, Other]),
            {error, Other}
    end.

-spec stop_memcache(memcache:pool_host(), memcache:pool_port()) -> ok | {error, term()}.
stop_memcache(Host, Port) ->
    Cmd = lists:flatten(io_lib:format(
              get_config_value(stop_memcached, default_command(stop_memcached)),
              [Host, Port]
          )),
    case os:cmd(Cmd) of
        [] ->
            timer:sleep(100),
            ok;
        Other ->
            ?WARNING("Unexpected return stoping memcached ~p: ~p", [{Host, Port}, Other]),
            {error, Other}
    end.

-spec default_command(start_memcached | stop_memcached) -> string().
default_command(start_memcached) ->
    "memcached -d -l ~s -m ~p -p ~p";
default_command(stop_memcached) ->
    "kill -9 $(ps aux | grep memcached | grep ~s | grep ~p | awk '{print $2}')".

-spec get_config_value(atom(), term()) -> term().
get_config_value(Key, Default) ->
    case application:get_env(memcache, Key) of
        undefined -> Default;
        {ok, Val} -> Val
    end.
