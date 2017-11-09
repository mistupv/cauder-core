-ifdef(debug).
-define(LOG(X), io:format("{~p,~p}: ~p~n", [?MODULE, ?LINE, X])).
-define(TO_STRING(X), lists:flatten(io_lib:format("~p",[X]))).
-else.
-define(LOG(X), true).
-define(TO_STRING(X), true).
-endif.

-define(FWD_SEM, fwd_sem).
-define(BWD_SEM, bwd_sem).

-define(TYPE_MSG,  msg).
-define(TYPE_PROC, proc).

-define(RULE_SEQ,      seq).
-define(RULE_CHECK,    check).
-define(RULE_SEND,     send).
-define(RULE_RECEIVE, 'receive').
-define(RULE_SPAWN,    spawn).
-define(RULE_SELF,     self).
-define(RULE_SCHED,    sched).

% ets defs
-define(SEM_REF, '_._app').

-define(FRESH_PID,  301).
-define(FRESH_TIME, 302).
-define(FRESH_VAR,  303).

-define(NOT_EXP,   not_exp).
-define(NULL_RULE, null_rule).
-define(NULL_OPT,  null_opt).

-record(proc, {pid,
               hist = [],
               env  = [],
               exp,
               mail = [],
               roll = []}).

-record(msg, {dest,
              val,
              time}).

-record(sys, {msgs  = [],
              procs = [],
              trace = []}).

-record(opt, {sem,    % forward or backward
              type,   % proc or msg
              id,     % integer
              rule}). % seq, spawn, ...

-record(trace, {type,
                from,
                to,
                val}).
