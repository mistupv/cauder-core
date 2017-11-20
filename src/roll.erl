%%%-------------------------------------------------------------------
%%% @doc Rollback operator for the reversible semantics for Erlang
%%% @end
%%%-------------------------------------------------------------------

-module(roll).
-export([can_roll/2, can_roll_send/2, eval_step/2, eval_roll_send/2]).

-include("cauder.hrl").

can_roll(#sys{procs = Procs}, Pid) ->
  case utils:pid_exists(Procs, Pid) of
    false -> false;
    true ->
      {Proc, _} = utils:select_proc(Procs, Pid),
      Hist = Proc#proc.hist,
      Mail = Proc#proc.mail,
      case {Hist, Mail} of
        {[], []} -> false;
        _ -> true
      end
  end.

eval_step(System, Pid) ->
  Procs = System#sys.procs,
  {Proc, _} = utils:select_proc(Procs, Pid),
  [CurHist|_]= Proc#proc.hist,  
  case CurHist of
    {send, _, _, DestPid, {_, Time}} ->
      ?LOG("ROLLing back SEND from " ++ ?TO_STRING(cerl:concrete(Pid)) ++ " to " ++ ?TO_STRING(cerl:concrete(DestPid))),
      roll_send(System, Pid, DestPid, Time);
    {spawn, _, _, SpawnPid} ->
      ?LOG("ROLLing back SPAWN of " ++ ?TO_STRING(cerl:concrete(SpawnPid))),
      roll_spawn(System, Pid, SpawnPid);
    _ ->
      RollOpts = roll_opts(System, Pid),
      cauder:eval_step(System, hd(RollOpts))
  end.

roll_send(System, Pid, OtherPid, Time) ->
  SendOpts = lists:filter(fun (X) -> X#opt.rule == ?RULE_SEND end,
                          roll_opts(System, Pid)),
  case SendOpts of
    [] ->
      SchedOpts = [ X || X <- roll_sched_opts(System, OtherPid),
                              X#opt.id == Time],
      case SchedOpts of
        [] ->
          NewSystem = eval_step(System, OtherPid),
          roll_send(NewSystem, Pid, OtherPid, Time);
        _ ->
          NewSystem = cauder:eval_step(System, hd(SchedOpts)),
          roll_send(NewSystem, Pid, OtherPid, Time)
      end;
    _ ->
      cauder:eval_step(System, hd(SendOpts))
  end.

roll_spawn(System, Pid, OtherPid) ->
  SpawnOpts = lists:filter(fun (X) -> X#opt.rule == ?RULE_SPAWN end,
                           roll_opts(System, Pid)),
  case SpawnOpts of
    [] ->
      NewSystem = eval_step(System, OtherPid),
      roll_spawn(NewSystem, Pid, OtherPid);
    _ ->
      cauder:eval_step(System, hd(SpawnOpts))
  end.

can_roll_send(System, Id) ->
  Procs = System#sys.procs,
  ProcsWithSend = utils:select_proc_with_send(Procs, Id),
  case length(ProcsWithSend) of
    0 -> false;
    _ -> true
  end.

eval_roll_send(System, Id) ->
  Procs = System#sys.procs,
  ProcsWithSend = utils:select_proc_with_send(Procs, Id),
  Proc = hd(ProcsWithSend),
  Pid = Proc#proc.pid,
  eval_roll_until_send(System, Pid, Id).

eval_roll_until_send(System, Pid, Id) ->
  Procs = System#sys.procs,
  {Proc, _} = utils:select_proc(Procs, Pid),
  [CurHist|_]= Proc#proc.hist,
  case CurHist of
    {send,_,_,_,{_, Id}} ->
      eval_step(System, Pid);
    _ ->
      NewSystem = eval_step(System, Pid),
      eval_roll_until_send(NewSystem, Pid, Id)
  end.

roll_opts(System, Pid) ->
  ProcOpts = roll_procs_opts(System, Pid),
  SchedOpts = roll_sched_opts(System, Pid),
  SchedOpts ++ ProcOpts.

roll_procs_opts(System, Pid) ->
  ProcOpts = bwd_sem:eval_procs_opts(System),
  utils:filter_options(ProcOpts, cerl:concrete(Pid)).

roll_sched_opts(System, Pid) ->
  #sys{procs = Procs} = System,
  {Proc, _} = utils:select_proc(Procs, Pid),
  SingleProcSys = #sys{procs = [Proc]},
  bwd_sem:eval_sched_opts(SingleProcSys).
