# erl-memcache

This library is a wrapper around poolboy and erlmc_conn. The first one is used to create a pool of connections, the latter 
library is included and creates a persistent connection to Memcached (binary protocol).

Features include:
 * automatically start a local Memcached instance
 * (named) pools and their management
 * statistics retrieval from Memcached


Example configuration:
```
[
	{memcache, 
		[{pools,
			[{yaddapool, [
                {size, 10},
                {max_overflow, 20},
                {port, 3333},
                {host, "localhost"},
                {opts, [{memory, 4}]},
                {start_server, true}
            ]}]
        }]
    }
]
```

Notes:
 * currently only the memory of a memcached instance (-m) can be set. 