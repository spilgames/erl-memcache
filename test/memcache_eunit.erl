-module(memcache_eunit).

-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

all_test_() ->
    {setup,
        fun() ->
            error_logger:tty(false),
            {ok, FConf} = elibs_application:set_env( memcache, pools, {pool1, [
                                                                           {size, 20},
                                                                           {max_overflow, 20},
                                                                           {host, "localhost"},
                                                                          {port, 11211}
                                                                       ]}),
            {ok, F} = memcache:start(),
            [F, FConf]
        end ,
        fun (Cleaners) ->
            lists:foreach(fun (F) -> F() end, Cleaners)
        end,
        [            
            ?_assertEqual(<<>>, memcache:flush()),  
            ?_assertEqual(<<>>, memcache:get(testkey752235565464453452)),       
            ?_assertEqual(<<"Not found">>, memcache:delete(testkey423980986345809388)),
            ?_assertEqual(<<>>, memcache:set(testkey662234300980881236, <<"testvalue">>)),
            ?_assertExit({{badarg,_},_}, memcache:set(testkey235230983420348394, 1)),
            ?_assertExit({{badarg,_},_}, memcache:set(testkey235230983420348394, "abc")),
            ?_assertExit({{badarg,_},_}, memcache:set(testkey235230983420348394, atom)),
                       
            ?_test(begin
                        ?assertMatch(<<>>, memcache:set(testkey234239982304983434, <<"testvalue">>)),
                        ?assertMatch(<<"testvalue">>, memcache:get(testkey234239982304983434)),
                        ?assertMatch(<<>>, memcache:delete(testkey234239982304983434)),
                        ?assertMatch(<<>>, memcache:get(testkey234239982304983434))
            end),
            
            ?_test(begin
                        ?assertMatch(<<>>, memcache:set(testkey884352345434356721, <<"testvalue">>, 1)),
                        ?assertMatch(<<"testvalue">>, memcache:get(testkey884352345434356721)),
                        timer:sleep(1200),
                        ?assertMatch(<<>>, memcache:get(testkey884352345434356721)) 
            end)
        ]}.
