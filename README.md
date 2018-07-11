# CauDEr

A causal-consistent replay debugger for Erlang

## Dependencies

This project uses [wx](http://erlang.org/doc/apps/wx/chapter.html), the Erlang binding of wxWidgets.  
Thus, you must have [built Erlang/OTP with wxWidgets](http://erlang.org/doc/installation_guide/INSTALL.html#Advanced-configuration-and-build-of-ErlangOTP_Building_Building-with-wxErlang).

## Compilation

First, compile the project:
```
make
```
Then, execute the script *cauder.sh* to start CauDEr
```
./cauder.sh
```
A user interface should appear then.

## How to use

To prepare a program for replay debugging, only two steps are needed:
 1. Load the program by using the menu item `File > Open`.
 2. Load the trace folder by using the menu item `File > Load Trace`.

Note that a _trace folder_ is required for replay debugging, which contains both general data about the trace (in `trace_result.log`) and distributed data about each process trace (i.e., their concurrent actions).

There are 3 panels (modes) for replay debugging an execution on the right side of the interface:
 * *Manual:* Type a pid in the textbox and use the buttons for going forward and backward in its execution. Note that this mode is not automatic, and some processes can be blocked due to non-
 * *Replay:* Type the required data (pid or message ids) for replaying _N steps_ in a single process, the spawning of a process, and the sending or receiving of a message. This mode is automatic, thus if you want to replay the receiving of a message (e.g., receiving the message with id `2`), all required actions for performing this receiving will be executed previously.
* *Rollback:* Type the required data (pid or message ids) for undoing _N steps_ in a single process, the spawning of a process, and the sending or receiving of a message. This mode is automatic, thus if you want to roll back the sending of a message (e.g., sending the message with id `2`), all required actions for rolling back this sending will be undone.

###Example

Supose we have traced the `proxy_server.erl` program and the trace results are in folder `proxy_server_trace/`.

First, we open (using the menu item `File > Open`) the file `proxy_server.erl`. This should compile the file and show its Core Erlang code in the Code tab.

Then, we load (using the menu item `File > Load Trace`) its trace folder `proxy_server_trace/`. Now the interface will show the State tab, and this means that computation has been started.

If we input `71` in the pid textbox (Manual mode), we will be able to run this process forward and backward (the available rule is enabled when it can be fired).

If we switch to Replay mode, we will be able replay computations automatically. For instance, if we input `3` in the message id textbox for _Replay send_ and then push the button, all (and only the) actions required for replaying the sending of the message will be done.
One can keep track of the performed actions by looking at the _Trace_ window.

In a similar way, if we switch to Rollback mode and type `73`, all (and only the) actions required for rolling back the spawning of that process will be performed. If any rollback has been triggered because of this request, it will be shown on the _Roll Log_ window.

## Limitations

Note that this software is a proof-of-concept implementation for replay debugging programs in Erlang. Some known limitations are:
 * The supported language is a first-order subset of Erlang (i.e., list comprehension or higher-order funtions are not supported).
 * Loading a trace can be done if the initial call has no arguments.
