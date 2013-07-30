-module(memcache_eunit).

-include_lib("eunit/include/eunit.hrl").

%%================================================================================================
%% TESTS
%%================================================================================================

application_start_test_() ->
    ?_test(begin
            error_logger:tty(false),
            {ok, F} = elibs_application:start(memcache),
            ?assertMatch(P when is_pid(P), whereis(memcache_sup)),
            ?assertMatch(P when is_pid(P), whereis(memcache_pools_sup)),
            ?assertMatch(P when is_pid(P), whereis(memcache)),
            ok = F(),
            ?assertMatch(undefined, whereis(memcache_sup)),
            ?assertMatch(undefined, whereis(memcache_pools_sup)),
            ?assertMatch(undefined, whereis(memcache))
        end).

single_pool_operation_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun (_) ->
                StartPoolRes=memcache:start_pool(testpool, "localhost", 3333, 10, 10, true),
                StartPoolSameName=memcache:start_pool(testpool, "localhost", 3334, 10, 10, true),
                PoolNotFoundOp=memcache:get(unknownpool, <<"key">>),
                EmptyGet=memcache:get(testpool, <<"key">>),
                EmptyGet2=memcache:get(testpool, key),
                Set=memcache:set(testpool, <<"key">>, <<"value">>),
                SetExpiring=memcache:set(testpool, <<"key3">>, <<"value3">>, 1),
                Readback=memcache:get(testpool, <<"key">>),
                Readback2=memcache:get(testpool, key),
                timer:sleep(1000),
                ReadbackExpired=memcache:get(testpool, key3),
                AnotherSet=memcache:set(testpool, anotherkey, <<"anothervalue">>),
                Delete=memcache:delete(testpool, anotherkey),
                DeleteNotFound=memcache:delete(testpool, asdasdasd),
                ReadbackDeleted=memcache:get(testpool, anotherkey),
                Flush=memcache:flush(testpool),
                ReadbackFlushed=memcache:get(testpool, <<"key">>),
                StopPool=memcache:stop_pool(testpool),
                StopPoolNotStarted=memcache:stop_pool(testpool2),
                [?_assertEqual(ok, StartPoolRes),
                 ?_assertEqual({error, poolname_in_use}, StartPoolSameName),
                 ?_assertEqual({error, pool_not_found}, PoolNotFoundOp),
                 ?_assertEqual({ok, <<>>}, EmptyGet),
                 ?_assertEqual({ok, <<>>}, EmptyGet2),
                 ?_assertEqual({ok, <<"value">>}, Set),
                 ?_assertEqual({ok, <<"value3">>}, SetExpiring),
                 ?_assertEqual({ok, <<"value">>}, Readback),
                 ?_assertEqual({ok, <<>>}, Readback2),
                 ?_assertEqual({ok, <<>>}, ReadbackExpired),
                 ?_assertEqual({ok, <<"anothervalue">>}, AnotherSet),
                 ?_assertEqual(ok, Delete),
                 ?_assertEqual(ok, DeleteNotFound),
                 ?_assertEqual({ok, <<>>}, ReadbackDeleted),
                 ?_assertEqual(ok, Flush),
                 ?_assertEqual({ok, <<>>}, ReadbackFlushed),
                 ?_assertEqual(ok, StopPool),
                 ?_assertEqual({error, pool_not_found}, StopPoolNotStarted)]
        end}.

pool_independence_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun (_) ->
                StartPool1=memcache:start_pool(testpool, "localhost", 3333, 10, 10, true),
                StartPool2=memcache:start_pool(testpool2, "localhost", 3334, 10, 10, true),
                Set1=memcache:set(testpool, key, <<"value">>),
                Set2=memcache:set(testpool2, key, <<"value2">>),
                Readback1=memcache:get(testpool, key),
                Readback2=memcache:get(testpool2, key),
                Flush1=memcache:flush(testpool),
                ReadbackFlushed1=memcache:get(testpool, key),
                ReadbackFlushed2=memcache:get(testpool2, key),
                GetPools=memcache:get_pools(),
                StopPool1=memcache:stop_pool(testpool),
                StopPool2=memcache:stop_pool(testpool2),
                [?_assertEqual(ok, StartPool1),
                 ?_assertEqual(ok, StartPool2),
                 ?_assertEqual({ok, <<"value">>}, Set1),
                 ?_assertEqual({ok, <<"value2">>}, Set2),
                 ?_assertEqual({ok, <<"value">>}, Readback1),
                 ?_assertEqual({ok, <<"value2">>}, Readback2),
                 ?_assertEqual(ok, Flush1),
                 ?_assertEqual({ok, <<>>}, ReadbackFlushed1),
                 ?_assertEqual({ok, <<"value2">>}, ReadbackFlushed2),
                 ?_assertEqual([testpool, testpool2], GetPools),
                 ?_assertEqual(ok, StopPool1),
                 ?_assertEqual(ok, StopPool2)]
        end}.

pools_cleanup_at_app_stop_test_() ->
    {setup, fun setup/0, fun (_) -> kill_memcached_instances() end,
     fun ([F]) ->
                ?_test(begin
                        ok=memcache:start_pool(testpool, "localhost", 3333, 10, 10, true),
                        ok=memcache:start_pool(testpool2, "localhost", 3334, 10, 10, true),
                        ok=F(),
                        ?assertEqual(
                            [],
                            os:cmd("ps aux | grep memcached | grep localhost"
                                   " | grep 3333 | awk '{print $2}'")
                        ),
                        ?assertEqual(
                            [],
                            os:cmd("ps aux | grep memcached | grep localhost"
                                   " | grep 3334 | awk '{print $2}'")
                        )
                    end)
        end}.

pools_get_restarted_by_sup_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun (_) ->
                ?_test(begin
                        ok=memcache:start_pool(testpool, "localhost", 3333, 10, 10, true),
                        kill_memcached_instances(),
                        % get call to a killed memcached to trigger the child restart and wait a bit
                        % for it to happen
                        ?assertMatch({error, {poolboy_error, _}}, memcache:get(testpool, any)),
                        ?assertEqual({ok, <<>>}, retry_get(testpool, any, 200, 10))
                    end)
        end}.

with_started_memcached_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun (_) ->
                ?_test(begin
                        ok=memcache_pools_sup:start_memcache("localhost", 3333),
                        ok=memcache:start_pool(testpool, "localhost", 3333, 10, 10, false),
                        ?assertEqual({ok, <<>>}, memcache:get(testpool, any))
                    end)
        end}.

port_verification_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun (_) ->
                ?_test(begin
                        ok=memcache:start_pool(testpool, "localhost", 3333, 10, 10, true),
                        ResOk=memcache_pools_sup:wait_for_memcache("localhost", 3333, 1, 100),
                        ResError=memcache_pools_sup:wait_for_memcache("localhost", 3334, 3, 100),
                        ?assertEqual(ok, ResOk),
                        ?assertMatch({error, {unable_to_connect_to_memcached, _, _}},
                                     ResError)
                    end)
        end}.

single_pool_restart_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun (_) ->
                {timeout, 30, [?_test(begin
                        lists:foreach(fun (_) ->
                                    ?assertMatch(ok, memcache:start_pool(testpool, "localhost",
                                                                         3333, 10, 10, true)),
                                    ?assertMatch(ok, memcache:stop_pool(testpool))
                            end, lists:seq(1, 20))
                    end)]}
        end}.

env_test_() ->
    {setup,
        fun () ->
                {ok, F1} = elibs_application:set_env(memcache, pools,
                    [{testpool, [
                                {size, 10},
                                {max_overflow, 20},
                                {port, 3333},
                                {host, "localhost"},
                                {start_server, true}
                            ]
                        }
                    ]),
                [F1] ++ setup()
        end,
        fun cleanup/1,
        [
            ?_assertEqual(
                ok,
                memcache_pools_sup:wait_for_memcache("localhost", 3333, 1, 100)
            )
        ]
    }.


%%================================================================================================
%% Internal functions
%%================================================================================================

setup() ->
    error_logger:tty(false),
    {ok, F} = memcache:start(),
    [F].

cleanup(L) ->
    lists:foreach(fun(F) -> F() end, L),
    kill_memcached_instances().

kill_memcached_instances() ->
    memcache_pools_sup:stop_memcache("localhost", 3333),
    memcache_pools_sup:stop_memcache("localhost", 3334).

retry_get(Pool, Key, _Interval, 0) ->
    memcache:get(Pool, Key);
retry_get(Pool, Key, Interval, Retries) ->
    case memcache:get(Pool, Key) of
        {ok, _}=Ok ->
            Ok;
        {error, {poolboy_error, _}} ->
            ?debugFmt("Retrying get(~p, ~p). ~p retries left with ~p ms interval",
                      [Pool, Key, Retries - 1, Interval]),
            timer:sleep(Interval),
            retry_get(Pool, Key, Interval, Retries - 1)
    end.

