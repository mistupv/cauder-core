%airline example

-module(airline).
-export([main/0,seatManager/1,booker/1]).
 
main() -> Pid=spawn(?MODULE,seatManager,[3]),
	  spawn(?MODULE,booker,[Pid]),
	  spawn(?MODULE,booker,[Pid]).

seatManager(NumOfSeats) ->
    receive {askForNum,Pid} ->
	    Pid!{num,NumOfSeats},seatManager(NumOfSeats);
	{book,Pid} -> io:format("Booked sit number ~w ~n",[NumOfSeats]),seatManager(NumOfSeats-1)
    end.

booker(Pid) -> Pid!{askForNum,self()}, 
	       receive {num,NumOfSeats} when
			 NumOfSeats > 0 -> Pid!{book,self()}, booker(Pid);
		   {num,0} -> 0
	       end.
