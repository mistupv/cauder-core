%%%-------------------------------------------------------------------
%%% @doc Some functions that implement the forward (reversible)
%%% semantics for Erlang. These can be divided into functions to get
%%% the evaluation options and functions to perform the evaluation
%%% @end
%%%-------------------------------------------------------------------

-module(fwd_sem).
-export([eval_step/2, eval_opts/1]).

-include("cauder.hrl").

eval_seq(Env,Exp) ->
  case is_list(Exp) of
    true -> eval_list(Env,Exp);
    false -> eval_seq_1(Env,Exp)
  end.

eval_seq_1(Env,Exp) ->
  case cerl:type(Exp) of
    var ->
      Value = proplists:get_value(Exp, Env),
      {Env,Value,tau};
    cons ->
      ConsHdExp = cerl:cons_hd(Exp),
      ConsTlExp = cerl:cons_tl(Exp),
      case is_exp(cerl:cons_hd(Exp)) of
        true ->
          {NewEnv,NewConsHdExp,Label} = eval_seq(Env,ConsHdExp),
          NewExp = cerl:c_cons_skel(NewConsHdExp,
                                    ConsTlExp);
        false ->
          {NewEnv,NewConsTlExp,Label} = eval_seq(Env,ConsTlExp),
          NewExp = cerl:c_cons_skel(ConsHdExp,
                                    NewConsTlExp)
      end,
      {NewEnv,NewExp,Label};
    values ->
      {NewEnv, NewValuesEs, Label} = eval_list(Env, cerl:values_es(Exp)),
      NewExp = cerl:c_values(NewValuesEs),
      {NewEnv, NewExp, Label};
    tuple ->
      {NewEnv, NewTupleEs, Label} = eval_list(Env, cerl:tuple_es(Exp)),
      NewExp = cerl:c_tuple_skel(NewTupleEs),
      {NewEnv, NewExp, Label};
    apply -> 
      ApplyArgs = cerl:apply_args(Exp),
      ApplyOp = cerl:apply_op(Exp),
      case is_exp(ApplyArgs) of
        true ->
          {NewEnv,NewApplyArgs,Label} = eval_seq(Env,ApplyArgs),
          NewExp = cerl:update_c_apply(Exp,
                                       ApplyOp,
                                       NewApplyArgs),
          {NewEnv,NewExp,Label};
        false ->
          FunDefs = ref_lookup(?FUN_DEFS),
          FunDef = utils:fundef_lookup(ApplyOp, FunDefs),
          NewFunDef = utils:fundef_rename(FunDef),
          FunBody = cerl:fun_body(NewFunDef),
          FunArgs = cerl:fun_vars(NewFunDef),
          % standard zip is used here (pretty-printer forces it)
          NewEnv = utils:merge_env(Env, lists:zip(FunArgs,ApplyArgs)),
          {NewEnv,FunBody,tau}
      end;
    'case' ->
      CaseArg = cerl:case_arg(Exp),
      case is_exp(CaseArg) of
        true ->
          {NewEnv,NewCaseArg,Label} = eval_seq(Env,CaseArg),
          NewExp = cerl:update_c_case(Exp,
                                      NewCaseArg,
                                      cerl:case_clauses(Exp)),
          {NewEnv,NewExp,Label};
        false ->
          %io:format("Env: ~p\n",[Env]),
          %io:format("CaseArg: ~p\n",[CaseArg]),
          CaseClauses = cerl:case_clauses(Exp),
          %io:format("CaseClauses: ~p\n",[CaseClauses]),
          CaseClauses2 = replace_guards(Env,CaseClauses),
          %io:format("CaseClauses2: ~p\n",[CaseClauses2]),
          %CaseClauses3 = init(CaseClauses2),
          CaseArgs =
            case cerl:type(CaseArg) of
              values -> cerl:values_es(CaseArg);
              _ -> [CaseArg]
          end,
          case cerl_clauses:reduce(CaseClauses2,CaseArgs) of
            {true,{Clause,Bindings}} ->
              ClauseBody = cerl:clause_body(Clause),
              NewEnv = utils:merge_env(Env, Bindings),
              {NewEnv,ClauseBody,tau};
            {false,_} ->
              io:fwrite("Error: No matching clause~n") 
          end
      end;
    'let' ->
      LetArg = cerl:let_arg(Exp),
      case is_exp(LetArg) of
        true ->
          {NewEnv,NewLetArg,Label} = eval_seq(Env,LetArg),
          NewExp = cerl:update_c_let(Exp,
                                     cerl:let_vars(Exp),
                                     NewLetArg,
                                     cerl:let_body(Exp)),
          {NewEnv,NewExp,Label};
        false ->
          LetVars = cerl:let_vars(Exp),
          LetEnv =
            case cerl:let_arity(Exp) of
              1 -> lists:zip(LetVars,[LetArg]);
              _ ->
                FlatLetArg =
                case cerl:type(LetArg) of
                  values ->
                    cerl:values_es(LetArg);
                  _ -> LetArg
                end,
                lists:zip(LetVars,FlatLetArg)
            end,
          NewEnv = utils:merge_env(Env, LetEnv),
          NewExp = cerl:let_body(Exp),
          {NewEnv,NewExp,tau}
      end;
    call ->
      CallArgs = cerl:call_args(Exp),
      CallModule = cerl:call_module(Exp),
      CallName = cerl:call_name(Exp),

      case is_exp(CallModule) of
        true ->
          {NewEnv,NewCallModule,Label} = eval_seq(Env,CallModule),
          NewExp = cerl:update_c_call(Exp,
                                      NewCallModule,
                                      CallName,
                                      CallArgs),
          {NewEnv,NewExp,Label};
        false ->
          case is_exp(CallName) of
            true ->
              {NewEnv,NewCallName,Label} = eval_seq(Env,CallName),
              NewExp = cerl:update_c_call(Exp,
                                          CallModule,
                                          NewCallName,
                                          CallArgs),
              {NewEnv,NewExp,Label};
            false ->
              case is_exp(CallArgs) of
                true ->
                  {NewEnv,NewCallArgs,Label} = eval_list(Env,CallArgs),
                  NewExp = cerl:update_c_call(Exp,
                                              CallModule,
                                              CallName,
                                              NewCallArgs),
                  {NewEnv,NewExp,Label};
                false ->
                  case {CallModule, CallName} of
                    {{c_literal,_,'erlang'},{c_literal,_,'spawn'}} ->
                      VarNum = ref_lookup(?FRESH_VAR),
                      ref_add(?FRESH_VAR, VarNum + 1),
                      Var = utils:build_var(VarNum),
                      FunName = lists:nth(2,CallArgs),
                      FunArgs = utils:list_from_core(lists:nth(3,CallArgs)),
                      {Env,Var,{spawn,{Var,FunName,FunArgs}}};
                    {{c_literal,_,'erlang'},{c_literal, _, 'self'}} ->
                      VarNum = ref_lookup(?FRESH_VAR),
                      ref_add(?FRESH_VAR, VarNum + 1),
                      Var = utils:build_var(VarNum),
                      {Env, Var, {self, Var}};
                    {{c_literal,_,'erlang'},{c_literal, _, '!'}} ->
                      DestPid = lists:nth(1, CallArgs),
                      MsgValue = lists:nth(2, CallArgs),
                      {Env, MsgValue, {send, DestPid, MsgValue}};
                    {{c_literal,_,'timer'},{c_literal,_,'sleep'}} ->
                      NewExp = cerl:c_atom('ok'),
                      {Env, NewExp, tau};
                    _ ->
                      ConcModule = cerl:concrete(CallModule),
                      ConcName = cerl:concrete(CallName),
                      ConcArgs = [utils:toErlang(Arg) || Arg <- CallArgs],
                      ConcExp = apply(ConcModule, ConcName, ConcArgs),
                      StrExp = lists:flatten(io_lib:format("~p", ([ConcExp]))) ++ ".",
                      {ok, ParsedExp, _} = erl_scan:string(StrExp),
                      {ok, TypedExp} = erl_parse:parse_exprs(ParsedExp),
                      CoreExp = hd([utils:toCore(Expr) || Expr <- TypedExp]),
                      NewExp = CoreExp,
                      {Env, NewExp, tau}
                  end
              end
          end
      end;
    seq ->
      SeqArg = cerl:seq_arg(Exp),
      case is_exp(SeqArg) of
        true ->
          {NewEnv,NewSeqArg,Label} = eval_seq(Env,SeqArg),
          NewExp = cerl:update_c_seq(Exp,
                                     NewSeqArg,
                                     cerl:seq_body(Exp)),
          {NewEnv,NewExp,Label};
        false ->
          NewExp = cerl:seq_body(Exp),
          {Env,NewExp,tau}
      end;
    'receive' ->
        VarNum = ref_lookup(?FRESH_VAR),
        ref_add(?FRESH_VAR, VarNum + 1),
        Var = utils:build_var(VarNum),
        % SubsExp = utils:substitute(Exp, Env),
        % {Env, Var, {rec, Var, cerl:receive_clauses(SubsExp)}}
        ReceiveClauses = cerl:receive_clauses(Exp),
        %%ReceiveClauses2 = replace_guards(Env,ReceiveClauses),
        {Env, Var, {rec, Var, ReceiveClauses}}
  end.

%init([_X]) -> [];
%nit([A|R]) -> [A|init(R)].

replace_guards(Bindings,Exps) ->
  lists:map(fun({c_clause,L,Pats,Guard,Exp}) -> 
          Guard2 = utils:replace_all(Bindings,Guard),
          Guard3 = eval_guard(Guard2),
          {c_clause,L,Pats,Guard3,Exp}
          %case ReducedGuard of
          %    {value,true} -> {c_clause,L,Pats,true,Exp};
          %    _Other -> {c_clause,L,Pats,ReducedGuard,Exp}
          %end 
        end, Exps).  

  eval_guard(Exp) ->
    case cerl:type(Exp) of
      call ->
	    CallArgs = cerl:call_args(Exp),
        CallModule = cerl:call_module(Exp),
        CallName = cerl:call_name(Exp),
        ConcModule = cerl:concrete(CallModule),
        ConcName = cerl:concrete(CallName),
        ConcArgs = [utils:toErlang(Arg) || Arg <- CallArgs],
        ConcExp = apply(ConcModule, ConcName, ConcArgs),
        StrExp = lists:flatten(io_lib:format("~p", ([ConcExp]))) ++ ".",
        %%io:format("ConcModule: ~p\nConcName: ~p\nConcArgs: ~p\nConcExp: ~p\nStrExp: ~p\n",[ConcModule,ConcName,ConcArgs,ConcExp,StrExp]),
        {ok, ParsedExp, _} = erl_scan:string(StrExp),
        {ok, TypedExp} = erl_parse:parse_exprs(ParsedExp),
        hd([utils:toCore(Expr) || Expr <- TypedExp]);
      _Other -> Exp
    end.

%%--------------------------------------------------------------------
%% @doc Performs an evaluation step in process Pid, given System
%% @end
%%--------------------------------------------------------------------
eval_step(System, Pid) ->
  Msgs = System#sys.msgs,
  Procs = System#sys.procs,
  Trace = System#sys.trace,
  {Proc, RestProcs} = utils:select_proc(Procs, Pid),
  #proc{pid = Pid, log = Log, hist = Hist, env = Env, exp = Exp} = Proc,
  {NewEnv, NewExp, Label} = eval_seq(Env, Exp),
  NewSystem = 
    case Label of
      tau ->
        NewProc = Proc#proc{hist = [{tau,Env,Exp}|Hist], env = NewEnv, exp = NewExp},
        System#sys{msgs = Msgs, procs = [NewProc|RestProcs]};
      {self, Var} ->
        NewHist = [{self, Env, Exp}|Hist],
        RepExp = utils:replace(Var, Pid, NewExp),
        NewProc = Proc#proc{hist = NewHist, env = NewEnv, exp = RepExp},
        System#sys{msgs = Msgs, procs = [NewProc|RestProcs]};
      {send, DestPid, MsgValue} ->
        {send, Time} = hd(Log),
        NewMsg = #msg{dest = DestPid, val = MsgValue, time = Time},
        NewMsgs = [NewMsg|Msgs],
        NewLog = tl(Log),
        NewHist = [{send, Env, Exp, DestPid, {MsgValue, Time}}|Hist],
        NewProc = Proc#proc{log = NewLog, hist = NewHist, env = NewEnv, exp = NewExp},
        TraceItem = #trace{type = ?RULE_SEND, from = Pid, to = DestPid, val = MsgValue, time = Time},
        NewTrace = [TraceItem|Trace],
        System#sys{msgs = NewMsgs, procs = [NewProc|RestProcs], trace = NewTrace};
      {spawn, {Var, FunName, FunArgs}} ->
        {spawn, PidNum} = hd(Log),
        SpawnPid = cerl:c_int(PidNum),
        ArgsLen = length(FunArgs),
        FunCall = cerl:c_var({cerl:concrete(FunName), ArgsLen}),
        % ReplayData = get(replay_data),
        % Path = ReplayData#replay.log_path,
        % PidText = integer_to_list(PidNum),
        {SpawnGProc, RestGProcs} =
          utils:select_proc(System#sys.ghosts, cerl:c_int(PidNum)),
        SpawnProc = #proc{pid = SpawnPid,
                          % log = utils:extract_pid_log_data(Path, PidText),
                          log = SpawnGProc#proc.log,
                          env = [],
                          exp = cerl:c_apply(FunCall,FunArgs),
                          spf = cerl:var_name(FunCall)},
        NewLog = tl(Log),
        NewHist = [{spawn, Env, Exp, SpawnPid}|Hist],
        RepExp = utils:replace(Var, SpawnPid, NewExp),
        NewProc = Proc#proc{hist = NewHist, log = NewLog, env = NewEnv, exp = RepExp},
        TraceItem = #trace{type = ?RULE_SPAWN, from = Pid, to = SpawnPid},
        NewTrace = [TraceItem|Trace],
        System#sys{msgs = Msgs, ghosts = RestGProcs, procs = [NewProc|[SpawnProc|RestProcs]], trace = NewTrace};
      {rec, Var, ReceiveClauses} ->
        {Bindings, RecExp, ConsMsg, NewMsgs} = matchrec(ReceiveClauses, Env, Log, Msgs),
        NewLog = tl(Log),
        UpdatedEnv = utils:merge_env(NewEnv, Bindings),
        RepExp = utils:replace(Var, RecExp, NewExp),
        NewHist = [{rec, Env, Exp, ConsMsg}|Hist],
        NewProc = Proc#proc{log = NewLog, hist = NewHist, env = UpdatedEnv, exp = RepExp},
        #msg{val = MsgValue, time = Time} = ConsMsg, 
        TraceItem = #trace{type = ?RULE_RECEIVE, from = Pid, val = MsgValue, time = Time},
        NewTrace = [TraceItem|Trace],
        System#sys{msgs = NewMsgs, procs = [NewProc|RestProcs], trace = NewTrace}
    end,
  NewSystem.

is_exp([]) -> false;
is_exp(Exp) when is_list(Exp) ->
  lists:any(fun is_exp/1, Exp);
is_exp(Exp) -> 
  case cerl:type(Exp) of
    literal -> false;
    nil -> false;
    cons -> is_exp(cerl:cons_hd(Exp)) or is_exp(cerl:cons_tl(Exp));
    values -> is_exp(cerl:values_es(Exp));
    tuple -> is_exp(cerl:tuple_es(Exp));
    _Other -> true
  end.

eval_list(Env,[Exp|Exps]) ->
  case is_exp(Exp) of
    true ->
      {NewEnv,NewExp,Label} = eval_seq(Env,Exp),
      {NewEnv,[NewExp|Exps],Label};
    false ->
      {NewEnv,NewExp,Label} = eval_list(Env,Exps),
      {NewEnv,[Exp|NewExp],Label}
  end.

matchrec(_, _, [], _) ->
  no_match;
matchrec(Clauses, Env, Log, Msgs) ->
  case Log of
    [] -> no_match;
    [TopLog | _] ->
      case TopLog of
        {'receive', Stamp} ->          
          case utils:check_msg(Msgs, Stamp) of
            none -> no_match;
            Msg ->
              MsgValue = Msg#msg.val,
              NewClauses =
                preprocessing_clauses(Clauses,MsgValue,Env),
              {true, {Clause, Bindings}} =
                cerl_clauses:reduce(NewClauses, [MsgValue]),
              ClauseBody = cerl:clause_body(Clause),
              NewMsgs = lists:delete(Msg, Msgs),
              {Bindings, ClauseBody, Msg, NewMsgs}
          end;
        _ -> no_match
      end
  end.

preprocessing_clauses(Clauses,Msg,Env) ->
  lists:map(fun({c_clause,L,Pats,Guard,Exp}) -> 
  	%io:format("Clauses: ~p~n",[Clauses]),
  	%io:format("match (Pats/[Msg]) ~p~n~p~n",[Pats,[Msg]]),
  	%io:format("--result: ~p~n",[cerl_clauses:match_list(Pats,[Msg])]),
    case cerl_clauses:match_list(Pats,[Msg]) of
      {true,Bindings} -> Guard2 = utils:replace_all(Bindings++Env,Guard),
      					 %io:format("calling eval_guard (Bindings/Guard/Guard2): ~n~p~n~p~n~p~n",[Bindings++Env,Guard,Guard2]),
                         Guard3 = eval_guard(Guard2),
                         {c_clause,L,Pats,Guard3,Exp};
      _ -> {c_clause,L,Pats,Guard,Exp}
    end
  end, Clauses).

%%--------------------------------------------------------------------
%% @doc Gets the evaluation options for a given System
%% @end
%%--------------------------------------------------------------------
eval_opts(System) ->
  ProcsOpts = eval_procs_opts(System),
  ProcsOpts.

eval_procs_opts(#sys{procs = []}) ->
  [];
eval_procs_opts(#sys{msgs = Msgs, procs = [CurProc|RestProcs]}) ->
  Pid = CurProc#proc.pid,
  Log = CurProc#proc.log,
  Exp = CurProc#proc.exp,
  Env = CurProc#proc.env,
    case eval_exp_opt(Exp, Env, Log, Msgs) of
    ?NOT_EXP ->
      eval_procs_opts(#sys{msgs = Msgs, procs = RestProcs});
    Opt ->
      [Opt#opt{sem = ?MODULE, type = ?TYPE_PROC, id = cerl:concrete(Pid)}|eval_procs_opts(#sys{msgs = Msgs, procs = RestProcs})]
  end.

eval_exp_opt(Exp, Env, Log, Msgs) ->
  case is_exp(Exp) of
    false ->
      ?NOT_EXP;
    true ->
      case cerl:type(Exp) of
        var ->
          #opt{rule = ?RULE_SEQ};
        cons ->
          ConsHdExp = cerl:cons_hd(Exp),
          ConsTlExp = cerl:cons_tl(Exp),
          case is_exp(ConsHdExp) of
            true ->
              eval_exp_opt(ConsHdExp, Env, Log, Msgs);
            false ->
              case is_exp(ConsTlExp) of
                true ->
                  eval_exp_opt(ConsTlExp, Env, Log, Msgs);
                false ->
                  ?NOT_EXP
              end
          end;
        values ->
          eval_exp_list_opt(cerl:values_es(Exp), Env, Log, Msgs);
        tuple ->
          eval_exp_list_opt(cerl:tuple_es(Exp), Env, Log, Msgs);
        apply ->
          ApplyArgs = cerl:apply_args(Exp),
          case is_exp(ApplyArgs) of
            true ->
              eval_exp_list_opt(ApplyArgs, Env, Log, Msgs);
            false ->
              #opt{rule = ?RULE_SEQ}
          end;
        'let' ->
          LetArg = cerl:let_arg(Exp),
          case is_exp(LetArg) of
            true ->
              eval_exp_opt(LetArg, Env, Log, Msgs);
            false ->
              #opt{rule = ?RULE_SEQ}
          end;
        seq ->
          SeqArg = cerl:seq_arg(Exp),
          case is_exp(SeqArg) of
            true ->
              eval_exp_opt(SeqArg, Env, Log, Msgs);
            false ->
              #opt{rule = ?RULE_SEQ}
          end;
        'case' ->
          CaseArg = cerl:case_arg(Exp),
          case is_exp(CaseArg) of
            true ->
              eval_exp_opt(CaseArg, Env, Log, Msgs);
            false ->
              #opt{rule = ?RULE_SEQ}
          end;
        call ->
          CallModule = cerl:call_module(Exp),
          case is_exp(CallModule) of
            true ->
              eval_exp_opt(CallModule, Env, Log, Msgs);
            false ->
              CallName = cerl:call_name(Exp),
              case is_exp(CallName) of
                true ->
                  eval_exp_opt(CallName, Env, Log, Msgs);
                false ->
                  CallArgs = cerl:call_args(Exp),
                  case is_exp(CallArgs) of
                    true ->
                      eval_exp_list_opt(CallArgs, Env, Log, Msgs);
                    false ->
                      case {CallModule, CallName} of
                        {{c_literal, _, 'erlang'},{c_literal, _, 'spawn'}} ->
                          case utils:check_log(Log) of
                            {spawn, _} ->
                              #opt{rule = ?RULE_SPAWN};
                            _ ->
                              ?NOT_EXP
                          end;
                        {{c_literal, _, 'erlang'},{c_literal, _, 'self'}} ->
                          #opt{rule = ?RULE_SELF};
                        {{c_literal, _, 'erlang'},{c_literal, _, '!'}} ->
                          case utils:check_log(Log) of
                            {send, _} ->
                              #opt{rule = ?RULE_SEND};
                            _ ->
                              ?NOT_EXP
                          end;
                        _ -> #opt{rule = ?RULE_SEQ}
                      end
                  end
              end
          end;
        'receive' ->
          ReceiveClauses = cerl:receive_clauses(Exp),
          case matchrec(ReceiveClauses, Env, Log, Msgs) of
            no_match ->
              ?NOT_EXP;
            _ ->
              #opt{rule = ?RULE_RECEIVE}
          end
      end
  end.

eval_exp_list_opt([], _, _, _) ->
  ?NOT_EXP;
eval_exp_list_opt([CurExp|RestExp], Env, Log, Msgs) ->
  case is_exp(CurExp) of
    true -> eval_exp_opt(CurExp, Env, Log, Msgs);
    false -> eval_exp_list_opt(RestExp, Env, Log, Msgs)
  end.

ref_add(Id, Ref) ->
    ets:insert(?APP_REF, {Id, Ref}).

ref_lookup(Id) ->
    ets:lookup_element(?APP_REF, Id, 2).
