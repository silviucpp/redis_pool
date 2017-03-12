redis_pool
================

A high performant Erlang pool for [eredis][1] based on [erlpool][2] 

Why another redis pool
------------------

Beside the fact that the most popular project in this area [eredis_pool][3] has a low activity in the last time, it has also the following problems:
  
- it's using another `gen_server` proxy from where get's the pid of the redis connection inside the `poolboy` checkout. This is
most probably because in the past `eredis` was crashing the `gen_server` when connection was lost and if too many crashes takes place
in a short period of time the supervisor could stop. This is no longer the case as time newer versions of eredis can reconnect without
crashing the `gen_server`.
- `poolboy` seems overkill for managing a pool of redis connections. Instead `erlpool` overhead is resumed only to one ETS `counter_update` operation and one `lookup`   


Quick start
-----------

Getting all deps and compile:

```
rebar get-deps
rebar compile
```

You can define your pools inside `app.config` :

```erlang
[
    {redis_pool, [
        {pools, [
            {mypool,[
                {size, 10},
                {host, "127.0.0.1"},
                {port, 6379},
                {database, undefined},
                {reconnect_sleep, 100}
            ]}
        ]}
    ]}
].
```

Or you can dynamically add/remove pools at runtime using `start_pool/2` and `stop_pool/1` :


```erlang 
redis_pool:start(),
Args = [
    {size, 20},
    {host, "127.0.0.1"},
    {port, 6379}
],

redis_pool:start_pool(mypool, Args),
{ok, <<"OK">>} = redis_pool:q(mypool, [<<"SET">>, <<"foo">>, <<"bar">>]).
```

The supported options for a pool are:

- `size` : the pool size (how many connections are created)
- `host` : redis server address (string, default: `"127.0.0.1"`)
- `port` : redis server port (integer, default: `6379`)
- `database` : database number (integer, default to `0`)
- `password` : database password (string, default empty password)
- `reconnect_sleep`: in case connection to server is lost after how many milliseconds should try to reconnect (integer, default to `100`)
- `connect_timeout`: how many milliseconds will stay in connect until timeout (integer, default `5000`)

The API is identical with the one from [eredis][1].

Benchmark:

Performance testing
-----------

The code is in `benchmark` folder. In the following test I send 300000 requests from 300 concurrent processes on a pool of 10 connections.

```
make bench
### 1355 ms 250000 req/sec  
```

You can run it yourself using `make bench` after you copy the `load_test.erl` from benchmark folder in src and compile. 

[1]:https://github.com/wooga/eredis
[2]:https://github.com/silviucpp/erlpool
[3]:https://github.com/hiroeorz/eredis_pool
