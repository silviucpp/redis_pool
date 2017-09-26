-module(redis_pool_utils).

-export([
    env/1
]).

env(Attr) ->
    case application:get_env(redis_pool, Attr) of
        {ok, Value} ->
            Value;
        _ ->
            undefined
    end.
