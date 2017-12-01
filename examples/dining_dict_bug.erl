-module(dining_dict_bug).
-export([main/0, waiter/1, philo/2]).


main() ->
  spawn(?MODULE, waiter, [self()]),
  receive
    table_prepared -> ok
  end.

spawn_philos(0, Dict, _) ->
  Dict;
spawn_philos(N, Dict, Pid) ->
  Pair = {spawn(?MODULE, philo, [Pid, N]), N},
  spawn_philos(N - 1, [Pair] ++ Dict , Pid).

make_forks(0, Forks) -> Forks;
make_forks(N, Forks) -> make_forks(N-1, [{N, free}] ++ Forks).

waiter(DiningPid) ->
  ForkDict = make_forks(5, []),
  PhiloDict = spawn_philos(5, [], self()),
  DiningPid ! table_prepared,
  waiter_1(ForkDict, PhiloDict).

waiter_1(ForkDict, PhiloDict) ->
    {UpdForkDict, UpdPhiloDict} =
      receive
      {hungry, PhiloPid} ->
        PhiloId = proplists:get_value(PhiloPid, PhiloDict),
        LeftForkId = PhiloId,
        RightForkId =  1 + (LeftForkId rem 5),
        LeftForkState = proplists:get_value(LeftForkId, ForkDict),
        RightForkState = proplists:get_value(RightForkId, ForkDict),
        case {LeftForkState, RightForkState} of
          {free, free} ->
            NewForkDict =
              pick_up_forks(PhiloPid, LeftForkId, RightForkId, ForkDict),
            {NewForkDict, PhiloDict};
          _ ->
            PhiloPid ! think,
            {ForkDict, PhiloDict}
        end;
      {eaten, PhiloPid} ->
        NewForkDict = put_down_forks(PhiloPid, ForkDict, PhiloDict),
        {NewForkDict, PhiloDict}
    end,
    waiter_1(UpdForkDict, UpdPhiloDict).

pick_up_forks(PhiloPid, LeftForkId, RightForkId, ForkDict) ->
  PhiloPid ! eat,
  TmpForkDict = lists:keyreplace(LeftForkId, 1, ForkDict, {LeftForkId, used}),
  NewForkDict = lists:keyreplace(RightForkId, 1, TmpForkDict, {RightForkId, used}),
  NewForkDict.

put_down_forks(PhiloPid, ForkDict, PhiloDict) ->
  PhiloId = proplists:get_value(PhiloPid, PhiloDict),
  LeftForkId  = PhiloId,
  % RightForkId = 1 + (LeftForkId rem 5), % Correct version
  RightForkId = 1 + (5 rem LeftForkId),   % Bugged version
  TmpForkDict = lists:keyreplace(LeftForkId, 1, ForkDict, {LeftForkId, free}),
  NewForkDict = lists:keyreplace(RightForkId, 1, TmpForkDict, {RightForkId, free}),
  NewForkDict.

philo(WaiterPid, PhiloId) ->
  think(PhiloId),
  request_until_eaten(WaiterPid, PhiloId),
  philo(WaiterPid, PhiloId).

think(PhiloId) ->
  io:fwrite("Philo " ++ integer_to_list(PhiloId) ++ " is thinking~n"),
  ThinkTime = rand:uniform(1000),
  timer:sleep(ThinkTime),
  ok.

eat(PhiloId) ->
  io:fwrite("Philo " ++ integer_to_list(PhiloId) ++ " has eaten~n"),
  timer:sleep(1000),
  ok.

request_until_eaten(WaiterPid, PhiloId) ->
    io:fwrite("Philo " ++ integer_to_list(PhiloId) ++ " is hungry~n"),
    WaiterPid ! {hungry, self()},
    receive
      think ->
        think(PhiloId),
        request_until_eaten(WaiterPid, PhiloId);
      eat ->
        eat(PhiloId),
        WaiterPid ! {eaten, self()}
    end.
