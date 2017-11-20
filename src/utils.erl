%%%-------------------------------------------------------------------
%%% @doc Utils functions for the reversible semantics for Erlang
%%% @end
%%%-------------------------------------------------------------------

-module(utils).
-export([fundef_lookup/2, fundef_rename/1, substitute/2,
         build_var/1, build_var/2, pid_exists/2,
         select_proc/2, select_msg/2,
         select_proc_with_time/2, select_proc_with_send/2,
         select_proc_with_spawn/2,
         list_from_core/1,
         update_env/2, merge_env/2,
         replace/3, pp_system/1, pp_trace/1, pp_roll_log/1,
         moduleNames/1,
         stringToFunName/1,stringToCoreArgs/1, toCore/1, toErlang/1,
         filter_options/2, filter_procs_opts/1,
         has_fwd/1, has_bwd/1, has_norm/1,
         is_queue_minus_msg/3, topmost_rec/1, last_msg_rest/1,
         gen_log_send/2, gen_log_spawn/2, empty_log/1]).

-include("cauder.hrl").

%%--------------------------------------------------------------------
%% @doc Searches a function definition in FunDefs with name FunName
%% @end
%%--------------------------------------------------------------------
fundef_lookup(FunName, FunDefs) ->
  {_, FunDef} = lists:keyfind(FunName, 1, FunDefs),
  FunDef.

%%--------------------------------------------------------------------
%% @doc Renames all the variables in function definition FunDef
%% @end
%%--------------------------------------------------------------------
fundef_rename(FunDef) ->
  FunVars = cerl:fun_vars(FunDef),
  FunBody = cerl:fun_body(FunDef),
  RenamedVars = pars_rename(FunVars),
  {RenamedExp, _} =
    cerl_trees:mapfold(fun (Exp, Acc) ->
                          case cerl:type(Exp) of
                            var ->
                              case cerl:var_name(Exp) of
                                {_FunName, _FunArity} ->
                                  NewExp = Exp,
                                  NewAcc = Acc;
                              OtherName ->
                                case lists:keyfind(Exp, 1, Acc) of
                                  false ->
                                    NewExp = fresh_var(OtherName),
                                    NewAcc = [{Exp,NewExp}] ++ Acc;
                                  {Exp, NewVar} ->
                                    NewExp = NewVar,
                                    NewAcc = Acc
                                end
                              end;
                            _Other ->
                              NewExp = Exp,
                              NewAcc = Acc
                          end,
                          {NewExp, NewAcc}
                        end,
                        RenamedVars,
                        FunBody),
  NewFunDef = cerl:c_fun([NewVar || {_, NewVar} <- RenamedVars], RenamedExp),
  NewFunDef.

pars_rename(Vars) ->
  [{Var, fresh_var(cerl:var_name(Var))} || Var <- Vars].

substitute(SuperExp, Env) ->
  cerl_trees:map(
    fun (Exp) ->
      case cerl:type(Exp) of
        var ->
          case proplists:get_value(Exp, Env) of
            undefined -> Exp;
            Value -> Value
          end;
        _   -> Exp
      end
    end, SuperExp).
%%--------------------------------------------------------------------
%% @doc Builds a variable from a given number Num
%% @end
%%--------------------------------------------------------------------
build_var(Num) ->
  NumAtom = list_to_atom("k_" ++ integer_to_list(Num)),
  cerl:c_var(NumAtom).

build_var(Name,Num) ->
  NumAtom = list_to_atom(atom_to_list(Name) ++ "_" ++ integer_to_list(Num)),
  cerl:c_var(NumAtom).

pid_exists(Procs, Pid) ->
  case [ P || P <- Procs, P#proc.pid == Pid] of
    [] -> false;
    _ -> true
  end.

%%--------------------------------------------------------------------
%% @doc Returns a tuple with a process with pid Pid from Procs and
%% the rest of processes from Procs
%% @end
%%--------------------------------------------------------------------
select_proc(Procs, Pid) ->
  [Proc] = [ P || P <- Procs, P#proc.pid == Pid],
  RestProcs = [ P || P <- Procs, P#proc.pid /= Pid],
  {Proc, RestProcs}.

%%--------------------------------------------------------------------
%% @doc Returns a tuple with a message with id Time from Msgs and
%% the rest of messages from Msgs
%% @end
%%--------------------------------------------------------------------
select_msg(Msgs, Time) ->
  [Msg] = [ M || M <- Msgs, M#msg.time == Time],
  RestMsgs = [ M || M <- Msgs, M#msg.time /= Time],
  {Msg, RestMsgs}.

%%--------------------------------------------------------------------
%% @doc Returns the process that contains a message with id Time
%% from Procs
%% @end
%%--------------------------------------------------------------------
select_proc_with_time(Procs, Time) ->
  ProcWithTime =
    lists:filter( fun (Proc) ->
                    Mail = Proc#proc.mail,
                    length([ ok || {_,MsgTime} <- Mail, MsgTime == Time]) > 0
                  end, Procs),
  hd(ProcWithTime).

%%--------------------------------------------------------------------
%% @doc Returns the processes that contain a send item in history
%% with time Time
%% @end
%%--------------------------------------------------------------------
select_proc_with_send(Procs, Time) ->
  ProcWithSend =
    lists:filter( fun (Proc) ->
                    Hist = Proc#proc.hist,
                    has_send(Hist, Time)
                  end, Procs),
  ProcWithSend.

%%--------------------------------------------------------------------
%% @doc Returns the processes that contain a spawn item in history
%% with pid Pid
%% @end
%%--------------------------------------------------------------------
select_proc_with_spawn(Procs, Pid) ->
  ProcWithSpawn =
    lists:filter( fun (Proc) ->
                    Hist = Proc#proc.hist,
                    has_spawn(Hist, Pid)
                  end, Procs),
  ProcWithSpawn.

%%--------------------------------------------------------------------
%% @doc Transforms a Core Erlang list to a regular list
%% @end
%%--------------------------------------------------------------------
list_from_core(Exp) ->
  case  cerl:is_c_nil(Exp) of
    true -> [];
    false -> [cerl:cons_hd(Exp)|list_from_core(cerl:cons_tl(Exp))]
  end.

%%--------------------------------------------------------------------
%% @doc Update the environment Env with a single binding
%% @end
%%--------------------------------------------------------------------
update_env({Key, Value}, Env) ->
  DelEnv = proplists:delete(Key, Env),
  DelEnv ++ [{Key, Value}].

%%--------------------------------------------------------------------
%% @doc Update the environment Env with multiple bindings
%% @end
%%--------------------------------------------------------------------
merge_env(Env, []) -> Env;
merge_env(Env, [CurBind|RestBind]) ->
  NewEnv = update_env(CurBind, Env),
  merge_env(NewEnv, RestBind).

%%--------------------------------------------------------------------
%% @doc Replaces a variable Var by SubExp (subexpression) in SuperExp
%% (expression)
%% @end
%%--------------------------------------------------------------------
replace(Var, SubExp, SuperExp) ->
  VarName = cerl:var_name(Var),
  cerl_trees:map(
    fun (Exp) ->
      case cerl:type(Exp) of
        var ->
          case cerl:var_name(Exp) of
            VarName -> SubExp;
            _Other -> Exp
          end;
        _Other -> Exp
      end
    end, SuperExp).

%%--------------------------------------------------------------------
%% @doc Pretty-prints a given System
%% @end
%%--------------------------------------------------------------------
pp_system(#sys{msgs = Msgs, procs = Procs}) ->
  [pp_msgs(Msgs),
   "\n",
   pp_procs(Procs)].

pp_msgs(Msgs) ->
  MsgsList = [pp_msg(Msg) || Msg <- Msgs],
  ["GM: [",
   string:join(MsgsList,","),
   "]\n"].

pp_procs(Procs) ->
  SortProcs = lists:sort(fun(P1, P2) -> P1#proc.pid < P2#proc.pid end, Procs),
  ProcsList = [pp_proc(Proc) || Proc <- SortProcs],
  string:join(ProcsList,"\n").

pp_msg(#msg{dest = DestPid, val = MsgValue, time = Time}) ->
  ["(",
   pp(DestPid),",{",
   pp(MsgValue),",",
   integer_to_list(Time),
   "})"].

pp_proc(#proc{pid = Pid, hist = Hist, env = Env, exp = Exp, mail = Mail}) ->
  [pp_pid(Pid),": ",
   pp_mail(Mail),"\n",
   pp_hist(Hist),"\n",
   pp_env(Env, Exp),"\n",
   pp(Exp),"\n"].

pp(CoreForm) -> core_pp:format(CoreForm).

pp_pid(Pid) ->
  ["P",pp(Pid)].

pp_env(Env, Exp) ->
  RelEnv =  rel_binds(Env,Exp),
  PairsList = [pp_pair(Var,Val) || {Var,Val} <- RelEnv],
  ["{",
   string:join(PairsList,", "),
   "}"].

pp_pair(Var,Val) ->
  [pp(Var)," -> ",pp(Val)].

is_send_rec({send,_,_,_,_}) -> true;
is_send_rec({rec,_,_,_,_}) -> true;
is_send_rec(_) -> false.

pp_hist(Hist) ->
  FiltHist = lists:filter(fun is_send_rec/1, Hist),
  StrItems = [pp_hist_1(Item) || Item <- FiltHist],
  ["[",string:join(StrItems, ","),"]"].

pp_hist_1({send,_,_,_,{Value,Time}}) ->
  ["send(",pp(Value),",",integer_to_list(Time),")"];
pp_hist_1({rec,_,_,{Value,Time},_}) ->
  ["rec(",pp(Value),",",integer_to_list(Time),")"].

pp_mail([]) -> "[]";
pp_mail(Mail) ->
  MailList = [pp_msg_mail(Val, Time) || {Val, Time} <- Mail],
  ["[",
   string:join(MailList,","),
   "]"].

pp_msg_mail(Val, Time) ->
  ["{",pp(Val),",",
   integer_to_list(Time),"}"].

%%--------------------------------------------------------------------
%% @doc Pretty-prints a given system trace
%% @end
%%--------------------------------------------------------------------
pp_trace(#sys{trace = Trace}) ->
  % Trace is built as a stack (newest item is first)
  % and we must reverse it to print it
  RevTrace = lists:reverse(Trace),
  TraceStr = [pp_trace_item(Item) || Item <- RevTrace],
  string:join(TraceStr,"\n").

pp_trace_item(#trace{type = Type,
                     from = From,
                     to   = To,
                     val  = Val,
                     time = Time}) ->
  case Type of
    ?RULE_SEND    -> pp_trace_send(From, To, Val, Time);
    ?RULE_SPAWN   -> pp_trace_spawn(From, To);
    ?RULE_RECEIVE -> pp_trace_receive(From, Val, Time)
  end.

pp_trace_send(From, To, Val, Time) ->
  [pp_pid(From)," sends ",pp(Val)," to ",pp_pid(To)," (",integer_to_list(Time),")"].

pp_trace_spawn(From, To) ->
  [pp_pid(From)," spawns ",pp_pid(To)].

pp_trace_receive(From, Val, Time) ->
  [pp_pid(From)," receives ",pp(Val)," (",integer_to_list(Time),")"].

%%--------------------------------------------------------------------
%% @doc Prints a given system roll log
%% @end
%%--------------------------------------------------------------------
pp_roll_log(#sys{roll = RollLog}) ->
  string:join(RollLog,"\n").

%%--------------------------------------------------------------------
%% @doc Returns the module names from Forms
%% @end
%%--------------------------------------------------------------------
moduleNames(Forms) ->
  FunDefs = cerl:module_defs(Forms),
  FunNames = [cerl:var_name(Var) || {Var,_Fun} <- FunDefs],
  FunNameStrings = [funNameToString({Name,Arity}) || {Name,Arity} <- FunNames, Name =/= 'module_info'],
  FunNameStrings.

funNameToString({Name,Arity}) ->
  atom_to_list(Name) ++ "/" ++ integer_to_list(Arity).

%%--------------------------------------------------------------------
%% @doc Converts a string String into a Core Erlang function name
%% @end
%%--------------------------------------------------------------------
stringToFunName(String) ->
  FunParts = string:tokens(String, "/"),
  Name = list_to_atom(lists:nth(1,FunParts)),
  Arity = list_to_integer(lists:nth(2,FunParts)),
  cerl:c_var({Name,Arity}).

%%--------------------------------------------------------------------
%% @doc Parses a string Str that represents a list of arguments
%% and transforms these arguments to their equivalent in Core Erlang
%% @end
%%--------------------------------------------------------------------
stringToCoreArgs([]) ->
  [];
stringToCoreArgs(Str) ->
  StrDot = Str ++ ".",
  {ok, ParsedStr, _} = erl_scan:string(StrDot),
  {ok, Exprs} = erl_parse:parse_exprs(ParsedStr),
  CoreExprs = [toCore(Expr) || Expr <- Exprs],
  CoreExprs.

%%--------------------------------------------------------------------
%% @doc Transforms an Erlang expression Expr to its equivalent in
%% Core Erlang
%% @end
%%--------------------------------------------------------------------
toCore(Expr) ->
  case Expr of
    {atom, _, Atom} ->
      cerl:c_atom(Atom);
    {integer, _, Int} ->
      cerl:c_int(Int);
    {float, _, Float} ->
      cerl:c_float(Float);
    {string, _, String} ->
      cerl:c_string(String);
    {tuple, _, TupleEs} ->
      cerl:c_tuple_skel([toCore(E) || E <- TupleEs]);
    {cons, _, Head, Tail} ->
      cerl:c_cons_skel(toCore(Head), toCore(Tail));
    {nil, _} ->
      cerl:c_nil()
  end.

toErlang(Expr) ->
  LitExpr =
    case cerl:is_literal(Expr) of
      true -> Expr;
      false -> cerl:fold_literal(Expr)
    end,
  cerl:concrete(LitExpr).

%%--------------------------------------------------------------------
%% @doc Filters the options with identifier Id
%% @end
%%--------------------------------------------------------------------
filter_options([], _) -> [];
filter_options([CurOpt|RestOpts], Id) ->
  #opt{id = OptId} = CurOpt,
  case (OptId == Id) of
    true -> [CurOpt|filter_options(RestOpts,Id)];
    false -> filter_options(RestOpts,Id)
  end.

%%--------------------------------------------------------------------
%% @doc Filters the process options from a list of Options
%% @end
%%--------------------------------------------------------------------
filter_procs_opts([]) -> [];
filter_procs_opts([CurOpt|RestOpts]) ->
  #opt{type = Type} = CurOpt,
  case Type of
    ?TYPE_MSG  -> filter_procs_opts(RestOpts);
    ?TYPE_PROC -> [CurOpt|filter_procs_opts(RestOpts)]
  end.

%%--------------------------------------------------------------------
%% @doc Returns true if a list of Options has a forward option,
%% and false otherwise
%% @end
%%--------------------------------------------------------------------
has_fwd([]) -> false;
has_fwd([#opt{sem = ?FWD_SEM}|_RestOpts]) -> true;
has_fwd([_CurOpt|RestOpts]) -> has_fwd(RestOpts).

%%--------------------------------------------------------------------
%% @doc Returns true if a list of Options has a backward option,
%% and false otherwise
%% @end
%%--------------------------------------------------------------------
has_bwd([]) -> false;
has_bwd([#opt{sem = ?BWD_SEM}|_RestOpts]) -> true;
has_bwd([_CurOpt|RestOpts]) -> has_bwd(RestOpts).

%%--------------------------------------------------------------------
%% @doc Returns true if a list of Options has a normalizing option,
%% and false otherwise
%% @end
%%--------------------------------------------------------------------
has_norm([]) -> false;
has_norm([#opt{sem = ?FWD_SEM, rule = Rule}|RestOpts]) ->
  case Rule of
    ?RULE_SCHED -> has_norm(RestOpts);
    _OtherRule -> true
  end;
has_norm([_CurOpt|RestOpts]) -> has_norm(RestOpts).

%%--------------------------------------------------------------------
%% @doc Returns true if Queue\Msg == OtherQueue, and false otherwise
%% @end
%%--------------------------------------------------------------------
is_queue_minus_msg(Queue, Msg, OtherQueue) ->
  ThisQueue = lists:delete(Msg, Queue),
  ThisQueue == OtherQueue.

%%--------------------------------------------------------------------
%% @doc Retrieves the topmost item in a history
%% @end
%%--------------------------------------------------------------------
topmost_rec([]) -> no_rec;
topmost_rec([CurHist|RestHist]) ->
  case CurHist of
    {rec,_,_,_,_} -> CurHist;
    _Other -> topmost_rec(RestHist)
  end.

has_send([], _) -> false;
has_send([{send,_,_,_,{_,Time}}|_], Time) -> true;
has_send([_|RestHist], Time) -> has_send(RestHist, Time).


has_spawn([], _) -> false;
has_spawn([{spawn,_,_,Pid}|_], Pid) -> true;
has_spawn([_|RestHist], Pid) -> has_spawn(RestHist, Pid).

fresh_var(Name) ->
  VarNum = ref_lookup(?FRESH_VAR),
  ref_add(?FRESH_VAR, VarNum + 1),
  utils:build_var(Name,VarNum).

last_msg_rest(Mail) ->
  LastMsg = lists:last(Mail),
  LenMail = length(Mail),
  RestMail = lists:sublist(Mail,LenMail-1),
  {LastMsg, RestMail}.

rel_binds(Env, Exp) ->
  RelVars = cerl_trees:variables(Exp),
  lists:filter( fun ({Var,_}) ->
                  VarName = cerl:var_name(Var),
                  lists:member(VarName,RelVars)
                end, Env).

gen_log_send(Pid, OtherPid) ->
  [["Roll send from ",pp_pid(Pid)," to ",pp_pid(OtherPid)]].

gen_log_spawn(_Pid, OtherPid) ->
  % [["Roll SPAWN of ",pp_pid(OtherPid)," from ",pp_pid(Pid)]].
  [["Roll spawn of ",pp_pid(OtherPid)]].

empty_log(System) ->
  System#sys{roll = []}.

ref_add(Id, Ref) ->
    ets:insert(?APP_REF, {Id, Ref}).

ref_lookup(Id) ->
    ets:lookup_element(?APP_REF, Id, 2).
