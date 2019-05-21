-module(exPat).
-export([ack/1,main/0]).

main() ->
    {1}!self(),
    ack(2).

ack(Num) -> 
  receive
    {Num} ->
      true;
    _ -> false 
  end. 
