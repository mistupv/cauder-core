%%%-------------------------------------------------------------------
%%% @doc Some functions that implement the backward (reversible)
%%% semantics for Erlang. These can be divided into functions to get
%%% the evaluation options and functions to perform the evaluation
%%% @end
%%%-------------------------------------------------------------------

-module(bwd_sem).
-export([eval_step/2, eval_opts/1, eval_procs_opts/1]).

-include("cauder.hrl").

%%--------------------------------------------------------------------
%% @doc Performs an evaluation step in process Pid, given System
%% @end
%%--------------------------------------------------------------------
eval_step(System, Pid) ->
  Procs = System#sys.procs,
  Msgs = System#sys.msgs,
  Trace = System#sys.trace,
  {Proc, RestProcs} = utils:select_proc(Procs, Pid),
  #proc{pid = Pid, log = Log, hist = [CurHist|RestHist]} = Proc,
  case CurHist of
    {tau, OldEnv, OldExp} ->
      OldProc = Proc#proc{hist = RestHist, env = OldEnv, exp = OldExp},
      System#sys{msgs = Msgs, procs = [OldProc|RestProcs]};
    {self, OldEnv, OldExp} ->
      OldProc = Proc#proc{hist = RestHist, env = OldEnv, exp = OldExp},
      System#sys{msgs = Msgs, procs = [OldProc|RestProcs]};
    {send, OldEnv, OldExp, DestPid, {MsgValue, Time}} ->
      {_Msg, RestMsgs} = utils:select_msg(Msgs, Time),
      OldLog = [{send,Time}|Log],
      OldProc = Proc#proc{log = OldLog, hist = RestHist, env = OldEnv, exp = OldExp},
      TraceItem = #trace{type = ?RULE_SEND, from = Pid, to = DestPid, val = MsgValue, time = Time},
      OldTrace = lists:delete(TraceItem, Trace),
      System#sys{msgs = RestMsgs, procs = [OldProc|RestProcs], trace = OldTrace};
    {spawn, OldEnv, OldExp, SpawnPid} ->
      {_SpawnProc, OldRestProcs} = utils:select_proc(RestProcs, SpawnPid),
      OldLog = [{spawn,cerl:concrete(SpawnPid)}|Log],
      OldProc = Proc#proc{log = OldLog, hist = RestHist, env = OldEnv, exp = OldExp},
      TraceItem = #trace{type = ?RULE_SPAWN, from = Pid, to = SpawnPid},
      OldTrace = lists:delete(TraceItem, Trace),
      System#sys{msgs = Msgs, procs = [OldProc|OldRestProcs], trace = OldTrace};
    {rec, OldEnv, OldExp, OldMsg} ->
      Time = OldMsg#msg.time,
      MsgValue = OldMsg#msg.val,
      OldLog = [{'receive', Time}|Log],
      OldProc = Proc#proc{log = OldLog, hist = RestHist, env = OldEnv, exp = OldExp},
      TraceItem = #trace{type = ?RULE_RECEIVE, from = Pid, val = MsgValue, time = Time},
      OldTrace = lists:delete(TraceItem, Trace),
      System#sys{msgs = [OldMsg|Msgs], procs = [OldProc|RestProcs], trace = OldTrace}
  end.

%%--------------------------------------------------------------------
%% @doc Gets the evaluation options for a given System
%% @end
%%--------------------------------------------------------------------
eval_opts(System) ->
  ProcsOpts = eval_procs_opts(System),
  ProcsOpts.

eval_procs_opts(System) ->
  Procs = System#sys.procs,
  Msgs = System#sys.msgs,
  ProcPairs = [utils:select_proc(Procs, Proc#proc.pid) || Proc <- Procs ],
  Opts = [eval_proc_opt(#sys{msgs = Msgs, procs = RestProcs}, Proc) ||  {Proc, RestProcs} <- ProcPairs],
  lists:filter( fun (X) ->
                  case X of
                    ?NULL_OPT -> false;
                    _Other -> true
                  end
                end, Opts).

eval_proc_opt(RestSystem, CurProc) ->
  RestProcs = RestSystem#sys.procs,
  Msgs = RestSystem#sys.msgs,
  Hist = CurProc#proc.hist,
  Rule =
    case Hist of
      [] ->
        ?NULL_RULE;
      [CurHist|_RestHist] ->
        case CurHist of
          {tau,_,_} ->  ?RULE_SEQ;
          {self,_,_} -> ?RULE_SELF;
          {send,_,_,_,{_, Time}} ->
            case utils:check_msg(Msgs, Time) of
              none -> ?NULL_RULE;
              _    -> ?RULE_SEND
            end;
          {spawn,_,_,SpawnPid} ->
            {SpawnProc, _RestProcs} = utils:select_proc(RestProcs, SpawnPid),
            #proc{hist = SpawnHist} = SpawnProc,
            case SpawnHist of
              [] -> ?RULE_SPAWN;
              _  -> ?NULL_RULE
            end;
          {rec,_,_, _} ->
              ?RULE_RECEIVE
        end
    end,
  case Rule of
    ?NULL_RULE -> ?NULL_OPT;
    OtherRule ->
      Pid = CurProc#proc.pid,
      #opt{sem = ?MODULE, type = ?TYPE_PROC, id = cerl:concrete(Pid), rule = OtherRule}
  end.

