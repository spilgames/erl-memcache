%% @doc memcached interface.
%%
%% this application provides an interface to a memcached server running by default on localhost.
%% it uses poolboy to effectively find available an available gen_server instance to talk to memcached.
%%
%% <pre>
%% 1> memcache:start().
%% Started memcache
%% {ok,#Fun&lt;elibs_application.2.45685040>}
%% 2> memcache:get(abc).
%% &lt;&lt;>>
%% 3> memcache:set(abc, term_to_binary({[1,2,3], at, "string"}) ).
%% &lt;&lt;>>
%% 4> memcache:get(abc).                                          
%% &lt;&lt;131,104,3,107,0,3,1,2,3,100,0,2,97,116,107,0,6,115,116,
%%   114,105,110,103>>
%% 5> binary_to_term(memcache:get(abc)).
%% {[1,2,3],at,"string"}
%%</pre>
-module(memcache).

-behaviour(application).
-behaviour(supervisor).

%% Application callbacks
-export([start/0, stop/0, start/2, stop/1, init/1]).

-export([flush/0, get/1, set/2, set/3, delete/1]).

-define(DEFAULT_EXPIRATION_TIME, 3600).

%% the name of the environment key in the memcache application settings.
-define(POOLNAME, pool1).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() -> elibs_application:start(?MODULE).
stop() -> application:stop(?MODULE).

start(_StartType, _StartArgs) ->
    elibs_application:load_extra_config(),
    supervisor:start_link({local, memcache_sup}, ?MODULE, []).

stop(_State) ->
    ok.

%% @doc initialize memcache gen_server.
%%
%% Typical initialization fragment in the config file. The pool1 <em>must</em> be the same as the POOLNAME define.
%% Host and port are optional in the configuration, but default to the values shown here.
%% <pre>
%%   {memcache, [
%%       {pools, [
%%           {pool1, [
%%               {size, 20},
%%               {max_overflow, 20},
%%               {host, "localhost"},
%%               {port, 11211}
%%           ]}
%%       ]}
%%   ]}
%%   </pre>

init([]) ->
    {ok, Pools} = application:get_env(memcache, pools),
    PoolSpecs = lists:map(fun({PoolName, PoolConfig}) ->
        Args = [{name, {local, PoolName}},
                {worker_module, erlmc_conn}]
                ++ PoolConfig,
        {PoolName, {poolboy, start_link, [Args]},
                    permanent, 5000, worker, [poolboy]}
    end, Pools),
    {ok, {{one_for_one, 10, 10}, PoolSpecs}}.

%%
%% @doc retrieve the value with a certain binary key.
%%
-spec get(term()) -> binary().
get(Key) ->
    Worker = poolboy:checkout(?POOLNAME),
    Reply = gen_server:call(Worker, {get, Key}),
    poolboy:checkin(?POOLNAME, Worker),
    Reply.


%%
%% @doc store the value with a certain binary key. 
%%
%% uses the default expiration time.
%%
-spec set(term(), binary() ) -> <<>>.
set(Key, Value) ->
    set(Key, Value, ?DEFAULT_EXPIRATION_TIME).

%%
%% @doc store the value with a certain binary key. 
%%
%% uses a custom expiration time.
%%
-spec set(term(), binary(), integer() ) -> <<>>.
set(Key, Value, Expiration) ->
    Worker = poolboy:checkout(?POOLNAME),
    Reply = gen_server:call(Worker, {set, Key, Value, Expiration}),
    poolboy:checkin(?POOLNAME, Worker),
    Reply.

%%
%% @doc delete the data associated with a certain key.
%%
%%
-spec delete(term() ) -> <<>>.
delete(Key) ->
    Worker = poolboy:checkout(?POOLNAME),
    Reply = gen_server:call(Worker, {delete, Key}),
    poolboy:checkin(?POOLNAME, Worker),
    Reply.

%%
%% @doc clear all items in memcached.
%%
%% DONT DO THIS unless you are absolutely sure that there are no other applications using stuff you stored in memcache.
-spec flush() -> <<>>.
flush() ->
    Worker = poolboy:checkout(?POOLNAME),
    Reply = gen_server:call(Worker, flush ),
    poolboy:checkin(?POOLNAME, Worker),
    Reply.

% vim:tw=120:ts=4:expandtab
