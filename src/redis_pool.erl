-module(redis_pool).
-author("silviu.caragea").

-include("redis_pool.hrl").

-export([
    start/0,
    start/1,
    stop/0,
    start_pool/2,
    stop_pool/1,
    q/2,
    q/3,
    qp/2,
    qp/3,
    q_noreply/2,
    q_async/2,
    q_async/3
]).

-spec start() -> ok  | {error, reason()}.

start() ->
    start(temporary).

-spec start(permanent | transient | temporary) -> ok | {error, reason()}.

start(Type) ->
    case application:ensure_all_started(redis_pool, Type) of
        {ok, _} ->
            ok;
        Other ->
            Other
    end.

-spec stop() -> ok.

stop() ->
    application:stop(redis_pool).

-spec start_pool(atom(), [redis_pool_option()]) -> ok | {error, reason()}.

start_pool(PoolName, PoolArgs0) ->
    Size = proplists:get_value(size, PoolArgs0, undefined),
    ok = erlpool:start_pool(PoolName, [
        {size, Size},
        {start_mfa, {eredis, start_link, [lists:keydelete(size, 1, PoolArgs0)]}}
    ]).

-spec stop_pool(atom()) -> ok | {error, reason()}.

stop_pool(PoolName) ->
    erlpool:stop_pool(PoolName).

-spec q(atom(), [any()]) ->
    {ok, return_value()} | {error, redis_error()}.

q(PoolName, Command) ->
    eredis:q(erlpool:pid(PoolName), Command).

-spec q(atom(), [any()], timeout()) ->
    {ok, return_value()} | {error, redis_error()}.

q(PoolName, Command, Timeout) ->
    eredis:q(erlpool:pid(PoolName), Command, Timeout).

-spec qp(atom(), pipeline()) ->
    [{ok, return_value()} | {error, binary()}] | {error, no_connection}.

qp(PoolName, Pipeline) ->
    eredis:qp(erlpool:pid(PoolName), Pipeline).

-spec qp(atom(), pipeline(), timeout()) ->
    [{ok, return_value()} | {error, binary()}] | {error, no_connection}.

qp(PoolName, Pipeline, Timeout) ->
    eredis:qp(erlpool:pid(PoolName), Pipeline, Timeout).

-spec q_noreply(atom(), [any()]) -> ok.

q_noreply(PoolName, Command) ->
    eredis:q_noreply(erlpool:pid(PoolName), Command).

-spec q_async(atom(), [any()]) -> ok.

q_async(PoolName, Command) ->
    eredis:q_async(erlpool:pid(PoolName), Command).

-spec q_async(atom(), [any()], pid()|atom()) -> ok.

q_async(PoolName, Command, Pid) ->
    eredis:q_async(erlpool:pid(PoolName), Command, Pid).
