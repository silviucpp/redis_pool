-module(redis_pool_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    ok = start_pools(),
    redis_pool_sup:start_link().

stop(_State) ->
    ok.

start_pools() ->
    FunPool = fun({Name, Args}) -> ok = redis_pool:start_pool(Name, Args) end,
    lists:foreach(FunPool, get_pools()).

get_pools() ->
    case redis_pool_utils:env(pools) of
        undefined ->
            [];
        Value ->
            Value
    end.