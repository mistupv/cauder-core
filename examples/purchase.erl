%FASE 2014 example

-module(purchase).
-export([main/0,asynchAnd/2,checkCredit/2,checkAddress/1,checkItem/1]).
 
main() -> Pid=spawn(?MODULE,asynchAnd,[2,self()]),
	  spawn(?MODULE,checkCredit,[15,Pid]),
	  spawn(?MODULE,checkAddress,[Pid]),
	  spawn(?MODULE,checkItem,[Pid]),
	  receive Result -> Result
	  end.

asynchAnd(N,Out) ->
    if 
	N > 0 ->   receive 
		       true -> asynchAnd(N-1,Out);
		       false -> Out!false
		   end;
	true -> Out!true
    end.

checkCredit(Price,Pid) -> if 
			      Price < 10 -> Pid!true;
			      true -> Pid!false
			  end.
					    
checkAddress(Pid) -> Pid!true.

checkItem(Pid) -> Pid!true.				   
     
     
     
