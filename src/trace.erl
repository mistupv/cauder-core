%%%-------------------------------------------------------------------
%%% @doc Rollback operator for the reversible semantics for Erlang
%%% @end
%%%-------------------------------------------------------------------

-module(trace).
-export([print_trace/1]).

-include("sem.hrl").

print_trace([]) -> [].
% print_trace([Tr|RestTrs]) ->
%   case Tr of
%     {send,From,To,Val} ->
%       print_send(Tr);
%     {spawn,From}
%   end.
