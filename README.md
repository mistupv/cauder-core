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
 1. Load the program by using File -> Open.
 2. Load the trace folder by using File -> Load Trace.

Note that a _trace folder_ is required for replay debugging, which contains both general data about the trace (in `trace_result.log`) and distributed data about each process trace (i.e., its concurrent actions).

There are 3 panels (modes) for replay debugging an execution on the right side of the interface:
 * Manual: Type a pid in the textbox and use the buttons for going forward and backward in its execution. Note that this mode is not automatic, and some processes can be blocked due to non-
 * Replay: Type the required for replaying N steps in a single process, the spawning of a process, and the sending or receiving of a message. This mode is automatic, thus if you want to replay the receiving of a message (e.g., receiving the message with id 2), all required actions for performing this receiving will be executed previously.
* Rollback: Type the required for undoing N steps in a single process, the spawning of a process, and the sending or receiving of a message. This mode is automatic, thus if you want to roll back the sending of a message (e.g., receiving the message with id 2), all required actions for performing this receiving will be undone.

## Limitations

Note that this software is a proof-of-concept implementation for replay debugging programs in Erlang. Some known limitations are:
 * The supported language is a first-order subset of Erlang (i.e., list comprehension or higher-order funtions are not supported).
 * Loading a trace can be done if the initial call has no arguments.
