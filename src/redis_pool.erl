-module(redis_pool).
-author("silviu.caragea").

-include("redis_pool.hrl").

-export([
    start/0,
    start/1,
    stop/0,
    start_pool/2,
    stop_pool/1,
    restart_pool/1,
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
        {group, redis_pool},
        {start_mfa, {eredis, start_link, [lists:keydelete(size, 1, PoolArgs0)]}}
    ]).

-spec stop_pool(atom()) -> ok | {error, reason()}.

stop_pool(PoolName) ->
    erlpool:stop_pool(PoolName).

-spec restart_pool(atom()) -> ok | {error, reason()}.

restart_pool(PoolName) ->
    erlpool:restart_pool(PoolName).

-spec q(atom(), [any()]) ->
    {ok, return_value()} | {error, redis_error()}.

q(PoolName, Command) ->
    try
        eredis:q(erlpool:pid(PoolName), Command)
    catch _:Error ->
        handle_exception(Error, Command)
    end.

-spec q(atom(), [any()], timeout()) ->
    {ok, return_value()} | {error, redis_error()}.

q(PoolName, Command, Timeout) ->
    try
        eredis:q(erlpool:pid(PoolName), Command, Timeout)
    catch _:Error ->
        handle_exception(Error, Command)
    end.

-spec qp(atom(), pipeline()) ->
    [{ok, return_value()} | {error, binary()}] | {error, redis_error()}.

qp(PoolName, Pipeline) ->
    try
        eredis:qp(erlpool:pid(PoolName), Pipeline)
    catch _:Error ->
        handle_exception(Error, Pipeline)
    end.

-spec qp(atom(), pipeline(), timeout()) ->
    [{ok, return_value()} | {error, binary()}] | {error, redis_error()}.

qp(PoolName, Pipeline, Timeout) ->
    try
        eredis:qp(erlpool:pid(PoolName), Pipeline, Timeout)
    catch _:Error ->
        handle_exception(Error, Pipeline)
    end.

-spec q_noreply(atom(), [any()]) ->
    ok | {error, redis_error()}.

q_noreply(PoolName, Command) ->
    try
        eredis:q_noreply(erlpool:pid(PoolName), Command)
    catch _:Error ->
        handle_exception(Error, Command)
    end.

-spec q_async(atom(), [any()]) ->
    ok | {error, redis_error()}.

q_async(PoolName, Command) ->
    try
        eredis:q_async(erlpool:pid(PoolName), Command)
    catch _:Error ->
        handle_exception(Error, Command)
    end.

-spec q_async(atom(), [any()], pid()|atom()) ->
    ok | {error, redis_error()}.

q_async(PoolName, Command, Pid) ->
    try
        eredis:q_async(erlpool:pid(PoolName), Command, Pid)
    catch _:Error ->
        handle_exception(Error, Command)
    end.

handle_exception({Error, _}, Command) ->
    {error, {Error, Command}};
handle_exception(Error, Command) ->
    {error, {Error, Command}}.
