-module(load_test).

-author("silviu.caragea").

-export([init/0, bench/2]).

init() ->
    redis_pool:start(),
    {ok, <<"OK">>} = redis_pool:q(mypool, [<<"SET">>, <<"foo">>, <<"bar">>]).

bench(Number, Concurrency) ->
    init(),
    Self = self(),
    List = lists:seq(1, Concurrency),
    LoopNumbers = Number div Concurrency,

    Fun = fun() -> {ok, <<"bar">>} = redis_pool:q(mypool, [<<"GET">>, <<"foo">>]) end,

    A = os:timestamp(),
    Pids = [spawn_link(fun() -> loop(LoopNumbers, Fun), Self ! {self(), done} end) || _ <- List],
    [receive {Pid, done} -> ok end || Pid <- Pids],
    B = os:timestamp(),

    print(Number, A, B).

print(Num, A, B) ->
    Microsecs = timer:now_diff(B, A),
    Time = Microsecs div Num,
    PerSec = case Time of
         0 ->
             "N/A";
         _ ->
             1000000 div Time
    end,

    io:format("### ~p ms ~p req/sec ~n", [Microsecs div 1000, PerSec]).

loop(0, _Fun) ->
    ok;
loop(Nr, Fun) ->
    Fun(),
    loop(Nr-1, Fun).
