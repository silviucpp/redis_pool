-module(redis_pool_utils).
-author("silviu.caragea").

-export([env/1]).

env(Attr) ->
    case application:get_env(redis_pool, Attr) of
        {ok, Value} ->
            Value;
        _ ->
            undefined
    end.
