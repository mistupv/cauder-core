-module(replay).
-export([can_replay/2, eval_step/2]).

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
    [] -> System;
    _  -> fwd_sem:eval_step(System, Pid)
  end.

% replay_spawn(System, Pid) ->
%   ParentPid = utils:search_spawn_parent(System, Pid),
%   replay_until_spawn(System, ParentPid).

% replay_until_spawn(System, ParentPid, Pid) ->
%   to_finish. 
