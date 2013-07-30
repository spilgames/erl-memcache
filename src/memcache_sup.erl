%% @doc
%% The main application supervisor. It manages both the memcache gen_server and the pools
%% supervisor with a one for all strategy.
-module(memcache_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(CHILD(Id, Mod, Type, Args), {Id, {Mod, start_link, Args},
                                     permanent, 5000, Type, [Mod]}).

%%%===================================================================
%%% API functions
%%%===================================================================

-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
    Children=[
        ?CHILD(memcache, memcache, worker, []),
        ?CHILD(memcache_pools_sup, memcache_pools_sup, supervisor, [])
    ],
    {ok, {{one_for_all, 5, 10}, Children}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
