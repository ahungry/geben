# Geben-1.1.0
Geben is a software package that interfaces Emacs to DBGp protocol
with which you can debug running scripts interactively. At this point,
the DBGp protocol is supported in several scripting languages with
help of custom extensions.

 * PHP with Xdebug 2.0.*
 * Perl, Python, Ruby and Tcl with Komodo Debugger Extensions

Currently Geben implements the following features.

 * continuation commands: run/stop/step-in/step-over/step-out
 * set/unset/listing breakpoints
 * expression evaluation
 * STDOUT/STDERR redirection
 * backtrace listing
 * variable inspection


# Requirements
[server side]
 - DBGp protocol enabled script engine, like:
   - PHP with Xdebug
   - Python with Komode Debugger Extension
   - etc.

[client side]
 - Emacs 24 or later


# Installation
[server side]

- To debug PHP scripts, you'll need to install PHP, Xdebug and
  optionally a web server.  Please visit their official sites to get
  packages and instructions of installation and configuration.
  PHP:    http://php.net
  Xdebug: http://xdebug.org

- To debug Perl, Python, Ruby and Tcl with Geben, Komodo
  Debugging Extensions will give you a big help.
  Distribution: http://aspn.activestate.com/ASPN/Downloads/Komodo/RemoteDebugging
  Documentation: http://aspn.activestate.com/ASPN/Reference/Products/Komodo/komodo-doc-debugger.html

[client side]

- M-x package-install geben [![MELPA](https://melpa.org/packages/geben-badge.svg)](https://melpa.org/#/geben)

# Debugging
For both modes, ensure you have enabled the xdebug extension.

## Command Line
To begin testing this out, create a simple PHP file
(/tmp/gebenTest.php) as such:

```php
<?php

$x = 1;
echo $x;
$y = 2;
echo $y;
```

You can now hop over to emacs, and enable the geben listener (default
port 9000) by running `M-x geben`.

At this point, you need to run the script with the xdebug
configuration set up to point to this listener.

Run this to invoke your script with xdebug dbgp enabled (or add to
your CLI php.ini file to ensure it runs with these settings every time):

```sh
 php -d xdebug.remote_enable=on \
     -d xdebug.remote_host=127.0.0.1 \
     -d xdebug.remote_port=9000 \
     -d xdebug.remote_handler=dbgp \
     -d xdebug.idekey=geben \
     -d xdebug.remote_autostart=On \
     /tmp/gebenTest.php
```

You'll notice the script doesn't complete, it pauses in your emacs
session.  Press `SPC` to step forward a line at a time.

Congratulations!  You've successfully done your first interactive
debugging session in geben.

## Web Based
Here is an illustration on PHP debugging.

1. Run Emacs.

2. Start geben, type: M-x geben

3. Access to the target PHP script page with any browser.
   You may need to add a query parameter `XDEBUG_SESSION_START' if you
   configured Xdebug to require manual trigger to start a remote
   debugging session.
   e.g.) http://www.example.com/test.php?XDEBUG_SESSION_START=1

4. Soon the server and Geben establish a debugging session
   connection. Then Emacs loads the script source code of the entry
   page in a buffer.

## More advanced usage
   You can control the debugger with several keys.

```conf
     - spc     step into/step over
     - i       step into
     - o       step over
     - r       step out
     - g       run
     - c       run to cursor
     - e       eval php expression
     - b       set a breakpoint at a line (see: defcustom geben-predefined-breakpoints)
     - B       set a breakpoint interactively
     - u       unset a breakpoint at a line
     - U       clear all breakpoints
     - \C-c b  display breakpoint list
     - >       set redirection mode
     - \C-u t  change redirection mode
     - d       display backtrace
     - t       display backtrace
     - v       display context variables
     - \C-c f  visit script file
     - w       where
     - q       stop
```

   When you hit any unbound key of `geben-mode', Geben will ask you to
   edit the original script file. Say yes and Geben will attempts to
   load the script file via `TRAMP'.

   To quit Geben, type: `M-x geben-end`

# Known Issues
* This version is not tested with Xdebug 2.1.* yet.

* There are some issues related Xdebug, version of at least 2.0.3.

  - Xdebug does not support STDERR command feature so that STDERR
    redirection feature does not work expectedly.

  - Xdebug does not implement `dbgp:' scheme feature so that with
    `step-in' command into a lambda function (you can create it with
    `create_function' in PHP) the cursor position is located at
    invalid line.

  - Xdebug may tell invalid line number on breaking by `return' type
    breakpoint. To this case Geben indicates the cursor at the top of
    the file in where the current breakpoint exists.

  - Xdebug unexpectedly breaks on returning from class/instance method
    if there is a `call' type breakpoint to the method.

  - If Xdebug is not loaded not as `zend_extension', some feature do
    not work as expectedly (e.g. step_into).


# Support
We always need your support - bug reports, feature requests,
and code/documents/design contributions.

To submit one or more of them, please file an issue here or email
Matthew Carter <m@ahungry.com>.
