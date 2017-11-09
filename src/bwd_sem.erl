%%%-------------------------------------------------------------------
%%% @doc Some functions that implement the backward (reversible)
%%% semantics for Erlang. These can be divided into functions to get
%%% the evaluation options and functions to perform the evaluation
%%% @end
%%%-------------------------------------------------------------------

-module(bwd_sem).
-export([eval_step/2, eval_sched/2, eval_opts/1,
         eval_procs_opts/1, eval_sched_opts/1]).

-include("cauder.hrl").

%%--------------------------------------------------------------------
%% @doc Performs an evaluation step in process Pid, given System
%% @end
%%--------------------------------------------------------------------
eval_step(#sys{msgs = Msgs, procs = Procs}, Pid) ->
  {Proc, RestProcs} = utils:select_proc(Procs, Pid),
  #proc{pid = Pid, hist = [CurHist|RestHist]} = Proc,
  case CurHist of
    {tau, OldEnv, OldExp} ->
      OldProc = Proc#proc{hist = RestHist, env = OldEnv, exp = OldExp},
      #sys{msgs = Msgs, procs = [OldProc|RestProcs]};
    {self, OldEnv, OldExp} ->
      OldProc = Proc#proc{hist = RestHist, env = OldEnv, exp = OldExp},
      #sys{msgs = Msgs, procs = [OldProc|RestProcs]};
    {send, OldEnv, OldExp, _DestPid, {_MsgValue, Time}} ->
      {_Msg, RestMsgs} = utils:select_msg(Msgs, Time),
      OldProc = Proc#proc{hist = RestHist, env = OldEnv, exp = OldExp},
      #sys{msgs = RestMsgs, procs = [OldProc|RestProcs]};
    {spawn, OldEnv, OldExp, SpawnPid} ->
      {_SpawnProc, OldRestProcs} = utils:select_proc(RestProcs, SpawnPid),
      OldProc = Proc#proc{hist = RestHist, env = OldEnv, exp = OldExp},
      #sys{msgs = Msgs, procs = [OldProc|OldRestProcs]};
    {rec, OldEnv, OldExp, _OldMsg, OldMail} ->
      OldProc = Proc#proc{hist = RestHist, env = OldEnv, exp = OldExp, mail = OldMail},
      #sys{msgs = Msgs, procs = [OldProc|RestProcs]}
  end.

%%--------------------------------------------------------------------
%% @doc Performs an evaluation step in message Id, given System
%% @end
%%--------------------------------------------------------------------
eval_sched(System, Id) ->
  #sys{msgs = Msgs, procs = Procs} = System,
  [{Proc,_}] = utils:select_proc_with_time(Procs, Id),
  Pid = Proc#proc.pid,
  {_, RestProcs} = utils:select_proc(Procs, Pid),
  [{Value, Id}|RestMsgs] = Proc#proc.mail,
  OldMsg = #msg{dest = Pid, val = Value, time = Id},
  OldProc = Proc#proc{mail = RestMsgs},
  OldMsgs = [OldMsg|Msgs],
  OldProcs = [OldProc|RestProcs],
  #sys{msgs = OldMsgs, procs = OldProcs}.

%%--------------------------------------------------------------------
%% @doc Gets the evaluation options for a given System
%% @end
%%--------------------------------------------------------------------
eval_opts(System) ->
  SchedOpts = eval_sched_opts(System),
  ProcsOpts = eval_procs_opts(System),
  SchedOpts ++ ProcsOpts.

eval_sched_opts(#sys{procs = Procs}) ->
  Opts = [eval_sched_opt(Proc) || Proc <- Procs],
  lists:filter(fun (X) ->
                  case X of
                    ?NULL_OPT -> false;
                    _ -> true
                  end
                end, Opts).

eval_procs_opts(System) ->
  #sys{msgs = Msgs, procs = Procs} = System,
  ProcPairs = [utils:select_proc(Procs, Proc#proc.pid) || Proc <- Procs ],
  Opts = [eval_proc_opt(#sys{msgs = Msgs, procs = RestProcs}, Proc) ||  {Proc, RestProcs} <- ProcPairs],
  lists:filter( fun (X) ->
                  case X of
                    ?NULL_OPT -> false;
                    _Other -> true
                  end
                end, Opts).

eval_proc_opt(#sys{msgs = Msgs, procs = RestProcs}, CurProc) ->
  Hist = CurProc#proc.hist,
  Rule =
    case Hist of
      [] ->
        ?NULL_RULE;
      [CurHist|_RestHist] ->
        case CurHist of
          {tau,_,_} ->  ?RULE_SEQ;
          {self,_,_} -> ?RULE_SELF;
          {send,_,_, DestPid, {MsgValue, Time}} ->
            MsgList = [ M || M <- Msgs, M#msg.time == Time,
                                        M#msg.dest == DestPid,
                                        M#msg.val == MsgValue ],
            case MsgList of
              [] -> ?NULL_RULE;
              _ -> ?RULE_SEND
            end;
          {spawn,_,_,SpawnPid} ->
            {SpawnProc, _RestProcs} = utils:select_proc(RestProcs, SpawnPid),
            #proc{hist = SpawnHist, mail = SpawnMail} = SpawnProc,
            case {SpawnHist, SpawnMail} of
              {[], []} -> ?RULE_SPAWN;
              _ -> ?NULL_RULE
            end;
          {rec,_,_, ConsMsg, OldMail} ->
            Mail = CurProc#proc.mail,
            case utils:is_queue_minus_msg(OldMail, ConsMsg, Mail) of
              true -> ?RULE_RECEIVE;
              false -> ?NULL_RULE
            end
        end
    end,
  case Rule of
    ?NULL_RULE -> ?NULL_OPT;
    OtherRule ->
      Pid = CurProc#proc.pid,
      #opt{sem = ?MODULE, type = ?TYPE_PROC, id = cerl:concrete(Pid), rule = OtherRule}
  end.

eval_sched_opt(Proc) ->
  #proc{hist = Hist, mail = Mail} = Proc,
  Rule =
    case Mail of
      [] -> ?NULL_RULE;
      [TopMsg|_RestMsgs] ->
        {_,Time} = TopMsg,
        TopRec = utils:topmost_rec(Hist),
        case TopRec of
          no_rec -> {?RULE_SCHED, Time};
          {rec,_,_,OldMsg,OldMail} ->
            case utils:is_queue_minus_msg(OldMail,OldMsg,[TopMsg|Mail]) of
              false -> {?RULE_SCHED, Time};
              true -> ?NULL_RULE
            end
        end
    end,
  case Rule of
    ?NULL_RULE -> ?NULL_OPT;
    {OtherRule, Id} ->
      #opt{sem = ?MODULE, type = ?TYPE_MSG, id = Id, rule = OtherRule}
  end.
