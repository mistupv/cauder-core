# Reversible Erlang

An implementation of a reversible semantics for Erlang.

## Dependencies

This project uses [wx](http://erlang.org/doc/apps/wx/chapter.html), the Erlang binding of wxWidgets.  
Therefore, you might have to [build Erlang/OTP with wxWidgets](http://erlang.org/doc/installation_guide/INSTALL.html#Advanced-configuration-and-build-of-ErlangOTP_Building_Building-with-wxErlang) if you did not in the past, but we recommend you to try to compile and execute the application first.

## How to use

First, compile the project:
```
make
```
Then, execute the script *rev-erlang.sh*:
```
./rev-erlang.sh
```
An astonishing graphical interface will appear in your screen.

![GUI screenshot](https://github.com/mistupv/rev-erlang/blob/screens/rev-erlang-init.png?raw=true)

To start using the application:
 1. Select an Erlang file using the File > Open option from the menu bar.
    This compiles the file and shows the Core Erlang code in the Code window.
 2. Choose the function to be evaluated and write its arguments (if needed).
 3. Push the START button.

Then, the initial state of the system will appear in the State window.

You can control the evaluation of a program using two modes:
 * **Manual**: Introduce a `Pid` in the `Pid/MsgId` text box and choose a fireable rule.
 * **Automatic**:
    * **Forward/Backward**: Introduce a number of steps `N` in the `Steps` text box and push the `Forward` or the `Backward` button. Then, `N` evaluation steps will be performed in the chosen direction.
    * **Normalize**: Move the system forward up to a system where the only firable rules are Sched (deliver a message).

Both modes can be used interchangeably.  

## Publications

This tool is an implementation of the improved version of a reversible semantics for Erlang (submitted for publication). A preliminary version of this semantics can be found in:
  * Naoki Nishida, Adrián Palacios and Germán Vidal. [A Reversible Semantics for Erlang](http://users.dsic.upv.es/~gvidal/german/lopstr16b/paper.pdf). To be published in *Proceedings of the 26th International Symposium on
Logic-Based Program Synthesis and Transformation*.

[comment]: # (Add pages and year once published --, 2016, pages 28:1-28:18)

## Have questions?

Check the [Wiki](https://github.com/mistupv/rev-erlang/wiki) for more information.
