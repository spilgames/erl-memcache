-module(memcache_pools_sup).

-behaviour(supervisor).

%% API
-export([add_pool/5,
         remove_pool/3,
         start_link/0
]).

%% Supervisor callbacks
-export([init/1]).

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
               memcache:pool_size(), memcache:pool_max_overflow()) -> {ok, pid()} | {error, term()}.
%% @end
add_pool(Poolname, Host, Port, Size, MaxOverflow) ->
    case start_memcache(Host, Port) of
        ok ->
            ChildSpec = build_child_spec(Poolname, Host, Port, Size, MaxOverflow),
            supervisor:start_child(?MODULE, ChildSpec);
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc
%% Terminates a pool and removes its childspec from this supervisor
-spec remove_pool(memcache:pool_name(), memcache:pool_host(), memcache:pool_port()) -> ok | {error, term()}.
%% @end
remove_pool(Poolname, Host, Port) ->
    ok=stop_memcache(Host, Port),
%    poolboy:stop(Poolname),
    case supervisor:terminate_child(?MODULE, Poolname) of
        ok -> 
            ok = supervisor:delete_child(?MODULE, Poolname);
        {error, not_found} ->
            {error, not_found}
    end.

%%%===================================================================
%%% Supervisor Callbacks
%%%===================================================================
init([]) ->
    {ok, {{one_for_one, 5, 10}, []}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec build_child_spec(memcache:pool_name(), memcache:pool_host(), memcache:pool_port(),
                       memcache:pool_size(), memcache:pool_max_overflow()) ->
    supervisor:child_spec().
build_child_spec(Poolname, Host, Port, Size, MaxOverflow) ->
    {Poolname, {poolboy, start_link, [[{name, {local, Poolname}},
                                       {worker_module, erlmc_conn},
                                       {size, Size},
                                       {max_overflow, MaxOverflow},
                                       {host, Host},
                                       {port, Port}]]},
                permanent, 5000, worker, [poolboy]}.

-spec start_memcache(memcache:pool_host(), memcache:pool_port()) -> ok | {error, term()}.
start_memcache(Host, Port) ->
    Cmd=elibs_string:format("memcached -d -l ~s -m 64 -p ~p", [Host, Port]),
    io:format("'~s'~n", [Cmd]),
    case os:cmd(Cmd) of
        [] ->
            timer:sleep(500),
            ok;
        Other ->
            {error, Other}
    end.

-spec stop_memcache(memcache:pool_host(), memcache:pool_port()) -> ok | {error, term()}.
stop_memcache(Host, Port) ->
    Cmd = elibs_string:format(
            "kill -9 $(ps aux | grep memcached | grep ~s | grep ~p | awk '{print $2}')",
            [Host, Port]
        ),
    io:format("'~s'~n", [Cmd]),
    case os:cmd(Cmd) of
        [] -> ok;
        Other -> {error, Other}
    end.


