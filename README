GEBEN-0.26 (Beta)
-----------------

GEBEN is a software package that interfaces Emacs to DBGp protocol
with which you can debug running scripts interactive. At this present
DBGp protocol are supported in several script languages with help of
custom extensions.

 * PHP with Xdebug 2.0.*
 * Perl, Python, Ruby and Tcl with Komodo Debugger Extensions

Currently GEBEN implements the following features.

 * continuation commands: run/stop/step-in/step-over/step-out
 * set/unset/listing breakpoints
 * expression evaluation
 * STDOUT/STDERR redirection
 * backtrace listing
 * variable inspection


REQUIREMENTS
------------

[server side]
 - DBGp protocol enabled script engine, like:
   - PHP with Xdebug
   - Python with Komode Debugger Extension
   - etc.

[client side]
 - Emacs22.1 or later


BIG CHANGES
-----------

- Since version 0.20 GEBEN does not require an external DBGp client
  program `debugclient'.

- At version 0.18 GEBEN dropped Emacs 21.4.

- Since version 0.1 GEBEN does not depend on CEDET emacs library.
  If you have installed CEDET previously only for GEBEN-pre-alpha,
  you can uninstall CEDET if you want.

- Now GEBEN lisp package forms a monolithic file.
  If you have installed previous version, you should remove all of old
  GEBEN files from installation directory.


INSTALLATION
------------

[server side]

- To debug PHP scripts, you'll need to install PHP, Xdebug and
  optionally a web server.  Please visit their official sites to get
  packages and instructions of installation and configuration.
  PHP:    http://php.net
  Xdebug: http://xdebug.org

- To debug Perl, Python, Ruby and Tcl with GEBEN, Komodo
  Debugging Extensions will give you a big help.
  Distribution: http://aspn.activestate.com/ASPN/Downloads/Komodo/RemoteDebugging
  Documentation: http://aspn.activestate.com/ASPN/Reference/Products/Komodo/komodo-doc-debugger.html

[client side]

1. Unpack GEBEN source code package and change directory to the
   unpacked directory.

<With GNU make command>
a. run `make'(or `gmake', depends on your environment).
b. If you are an administrator, Run: sudo make install
b' Or Run: SITELISP=$HOME/path/to/install make install

<Without GNU make command>
a. Byte compile 'dbgp.el'.
b. Byte compile `geben.el'.
c. Copy `dbgp.elc', `geben.elc' and entire `tree-widget' directory to
   any directory where Emacs can find.(Or add the path to `load-path'
   list)

<common>
2. Insert autoload hooks into your .Emacs file.
    -> (autoload 'geben "geben" "PHP Debugger on Emacs" t)
3. Restart Emacs.


DEBUGGING
---------

Here is an illustration on PHP debugging.

1. Run Emacs.

2. Start geben, type: M-x geben

3. Access to the target PHP script page with any browser.
   You may need to add a query parameter `XDEBUG_SESSION_START' if you
   configured Xdebug to require manual trigger to start a remote
   debugging session.
   e.g.) http://www.example.com/test.php?XDEBUG_SESSION_START=1

4. Soon the server and GEBEN establish a debugging session
   connection. Then Emacs loads the script source code of the entry
   page in a buffer.

5. Now the buffer is under the minor-mode 'geben-mode'.
   You can control the debugger with several keys.

     spc     step into/step over
     i       step into
     o       step over
     r       step out
     g       run
     c       run to cursor
     b       set a breakpoint at a line
     B       set a breakpoint interactively
     u       unset a breakpoint at a line
     U       clear all breakpoints
     \C-c b  display breakpoint list
     >       set redirection mode
     \C-u t  change redirection mode
     d       display backtrace
     t       display backtrace
     v       display context variables
     \C-c f  visit script file
     w       where
     q       stop

   When you hit any unbound key of `geben-mode', GEBEN will ask you to
   edit the original script file. Say yes and GEBEN will attempts to
   load the script file via `TRAMP'.

6. If you felt you'd debugged enough, it's time to quit GEBEN.
   To quit GEBEN, type: M-x geben-end

Known Issues
------------

* This version is not tested with Xdebug 2.1.* yet.

* There are some issues related Xdebug, version of at least 2.0.3.

  - Xdebug does not support STDERR command feature so that STDERR
    redirection feature does not work expectedly.

  - Xdebug does not implement `dbgp:' scheme feature so that with
    `step-in' command into a lambda function (you can create it with
    `create_function' in PHP) the cursor position is located at
    invalid line.

  - Xdebug may tell invalid line number on breaking by `return' type
    breakpoint. To this case GEBEN indicates the cursor at the top of
    the file in where the current breakpoint exists.

  - Xdebug unexpectedly breaks on returning from class/instance method
    if there is a `call' type breakpoint to the method.

  - If Xdebug is not loaded not as `zend_extension', some feature do
    not work as expectedly (e.g. step_into).
    

SUPPORT
-------

We all time need your supports - bug reports, feature requests,
code/documents/design contributions, and donations.
To submit one or more of them, please visit our web site.

 http://code.google.com/p/geben-on-emacs/

Also there are mailinglists.

For usage questions:
 http://groups.google.com/group/geben-users

For package contributions:
 http://groups.google.com/group/geben-dev

Your posts will make GEBEN development progress.

Thank you.

--

reedom <fujinaka.tohru@gmail.com>
