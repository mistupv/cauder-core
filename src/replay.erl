-module(replay).
-export([can_replay/2, eval_step/2, can_replay_spawn/2,
         replay_spawn/2, can_replay_send/2, replay_send/2,
         can_replay_rec/2, replay_rec/2]).

-include("cauder.hrl").

can_replay(#sys{procs = Procs}, Pid) ->
  case utils:pid_exists(Procs, Pid) of
    false -> false;
    true ->
      {Proc, _} = utils:select_proc(Procs, Pid),
      Log = Proc#proc.log,
      % Mail = Proc#proc.mail,
      % case {Hist, Mail} of
      case utils:check_log(Log) of
        none -> false;
        _ -> true
      end
  end.

eval_step(System, Pid) ->
  Opts = fwd_sem:eval_opts(System),
  FiltOpts = [Opt || Opt <- Opts, Opt#opt.id == cerl:concrete(Pid)],
  case FiltOpts of
    [] ->
        {Proc, _} = utils:select_proc(System#sys.procs, Pid),
        Log = Proc#proc.log,
        Request = utils:check_log(Log),
        case Request of
          {'receive', Stamp} ->
            replay_send(System, Stamp),
            replay_rec(System, Stamp)
        end;
    _  ->
      fwd_sem:eval_step(System, Pid)
  end.

replay_spawn(System, Pid) ->
  Procs = System#sys.procs,
  Ghosts = System#sys.ghosts,
  case utils:pid_exists(Procs, Pid) of
    false ->
      [ParentPid] =
        utils:search_spawn_parent(Procs, cerl:concrete(Pid)) ++
        utils:search_spawn_parent(Ghosts, cerl:concrete(Pid)),
        replay_until_spawn(System, ParentPid, Pid);
    true ->
      System
  end.

replay_until_spawn(System, ParentPid, Pid) ->
  SpawnSystem = replay_spawn(System, ParentPid),
  {PProc, _} = utils:select_proc(SpawnSystem#sys.procs, ParentPid),
  ParentLog = PProc#proc.log,
  case lists:member({spawn,cerl:concrete(Pid)}, ParentLog) of
    false -> SpawnSystem;
    true -> replay_until_spawn_1(SpawnSystem, ParentPid, Pid)
  end.

replay_until_spawn_1(System, ParentPid, Pid) ->
  NewSystem = eval_step(System, ParentPid),
  {PProc, _} = utils:select_proc(NewSystem#sys.procs, ParentPid),
  ParentLog = PProc#proc.log,
  CPid = cerl:concrete(Pid),
  case lists:member({spawn,CPid}, ParentLog) of
    false ->
      NewSystem;
    true ->
      replay_until_spawn_1(NewSystem, ParentPid, Pid)
  end.

replay_send(System, Stamp) ->
  Procs = System#sys.procs,
  Ghosts = System#sys.ghosts,
  % Msgs = System#sys.msgs,
  case can_replay_send(System, Stamp) of
    true ->
        [SenderPid] = utils:search_msg_sender(Procs, Stamp) ++
                      utils:search_msg_sender(Ghosts, Stamp),
        replay_until_send(System, SenderPid, Stamp);
    false ->
      System
  end.

replay_until_send(System, SenderPid, Stamp) ->
  NewSystem = replay_spawn(System, SenderPid),
  {SProc, _} = utils:select_proc(NewSystem#sys.procs, SenderPid),
  SenderLog = SProc#proc.log,
  case lists:member({send,Stamp}, SenderLog) of
    false ->
      NewSystem;
    true ->
      replay_until_send_1(NewSystem, SenderPid, Stamp)
  end.

replay_until_send_1(System, SenderPid, Stamp) ->
  NewSystem = eval_step(System, SenderPid),
  {SProc, _} = utils:select_proc(NewSystem#sys.procs, SenderPid),
  SenderLog = SProc#proc.log,
  case lists:member({send,Stamp}, SenderLog) of
    false ->
      NewSystem;
    true ->
      replay_until_send_1(NewSystem, SenderPid, Stamp)
  end.

replay_rec(System, Stamp) ->
  Procs = System#sys.procs,
  Ghosts = System#sys.ghosts,
  case can_replay_rec(System, Stamp) of
    true ->
            [RecPid] = utils:search_msg_receiver(Procs, Stamp) ++
                       utils:search_msg_receiver(Ghosts, Stamp),
            replay_until_rec(System, RecPid, Stamp);
    false ->
      System
  end.

replay_until_rec(System, ReceiverPid, Stamp) ->
  NewSystem = replay_spawn(System, ReceiverPid),
  NewSystem0 = replay_send(NewSystem, Stamp),
  {RProc, _} = utils:select_proc(NewSystem0#sys.procs, ReceiverPid),
  ReceiverLog = RProc#proc.log,
  case lists:member({'receive',Stamp}, ReceiverLog) of
    false ->
      System;
    true ->
      replay_until_rec_1(NewSystem0, ReceiverPid, Stamp)
  end.

replay_until_rec_1(System, ReceiverPid, Stamp) ->
  NewSystem = eval_step(System, ReceiverPid),
  {RProc, _} = utils:select_proc(NewSystem#sys.procs, ReceiverPid),
  ReceiverLog = RProc#proc.log,
  case lists:member({'receive',Stamp}, ReceiverLog) of
    false ->
      NewSystem;
    true ->
      replay_until_rec_1(NewSystem, ReceiverPid, Stamp)
  end.

can_replay_spawn(System, Pid) ->
  Procs = System#sys.procs,
  Ghosts = System#sys.ghosts,
  ParentProcs =
    utils:search_spawn_parent(Procs, cerl:concrete(Pid)) ++
    utils:search_spawn_parent(Ghosts, cerl:concrete(Pid)),
  case ParentProcs of
    [] -> false;
    _ -> true
  end.

can_replay_send(System, Stamp) ->
  Procs = System#sys.procs,
  Ghosts = System#sys.ghosts,
  SendProcs = utils:search_msg_sender(Procs, Stamp) ++
              utils:search_msg_sender(Ghosts, Stamp),
  case SendProcs of
    [] -> false;
    _ -> true
  end.


can_replay_rec(System, Stamp) ->
  Procs = System#sys.procs,
  Ghosts = System#sys.ghosts,
  RecProcs = utils:search_msg_receiver(Procs, Stamp) ++
              utils:search_msg_receiver(Ghosts, Stamp),
  case RecProcs of
    [] -> false;
    _ -> true
  end.
