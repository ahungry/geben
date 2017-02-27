# Geben
Geben is a software package that interfaces Emacs to DBGp protocol
with which you can debug running scripts interactively. At this point,
the DBGp protocol is supported in several scripting languages with
help of custom extensions.

 * PHP with Xdebug 2.*
 * Perl, Python, Ruby and Tcl with Komodo Debugger Extensions

[![MELPA](https://melpa.org/packages/geben-badge.svg)](https://melpa.org/#/geben)

<!-- markdown-toc start - Do not edit this section. Run M-x markdown-toc-generate-toc again -->
**Table of Contents**

- [Geben](#geben)
    - [Features](#features)
- [Installation](#installation)
    - [PHP](#php)
        - [Command Line](#command-line)
        - [Web](#web)
        - [VMs](#vms)
    - [Perl](#perl)
    - [Python](#python)
    - [Ruby](#ruby)
    - [Tcl](#tcl)
- [Usage](#usage)
- [About](#about)
    - [Known Issues](#known-issues)
        - [PHP](#php)
    - [Support](#support)
    - [License](#license)

<!-- markdown-toc end -->

## Features
 * continuation commands: run/stop/step-in/step-over/step-out
 * set/unset/listing breakpoints
 * expression evaluation
 * STDOUT/STDERR redirection
 * backtrace listing
 * variable inspection

# Installation
We recommend installing geben via melpa: `M-x package-install geben`.

Alternatively clone this repository and add it to your `load-path`.

## PHP
To debug PHP scripts, you will need to install PHP, [Xdebug](http://xdebug.org) and optionally a web server.  Please visit their official sites to get packages and instructions of installation and configuration:

### Command Line
To begin testing this out, create a simple PHP file (/tmp/gebenTest.php) as such:

```php
<?php
$x = 1;
echo $x;
$y = 2;
echo $y;
```

You can now hop over to emacs, and enable the geben listener (default port 9000) by running `M-x geben`.

At this point, you need to run the script with the xdebug configuration set up to point to this listener.

Run this to invoke your script with xdebug dbgp enabled (or add to your CLI php.ini file to ensure it runs with these settings every time):

```sh
 php -d xdebug.remote_enable=on \
     -d xdebug.remote_host=127.0.0.1 \
     -d xdebug.remote_port=9000 \
     -d xdebug.remote_handler=dbgp \
     -d xdebug.idekey=geben \
     -d xdebug.remote_autostart=On \
     /tmp/gebenTest.php
```

You will notice the script does not complete, it pauses in your emacs session.  Press `SPC` to step forward a line at a time.

### Web

1. Run Emacs.

2. Start geben `M-x geben`

3. Access to the target PHP script page with any browser.
   You may need to add a query parameter `XDEBUG_SESSION_START` if you configured Xdebug to require manual trigger to start a remote debugging session.
   e.g. http://www.example.com/test.php?XDEBUG_SESSION_START=1

4. Soon the server and Geben establish a debugging session
   connection. Then Emacs loads the script source code of the entry
   page in a buffer.

### VMs
1. Add a mapping from files on the host to files on the vm/docker image with `M-x customize-variable geben-path-mappings`
or set in emacs config with `(setq geben-path-mappings '(("<project base on host>" "<project base on vm>"))`

2. Check those mappings with `C-h v geben-path-mappings`. Beware that the mappings only show up after geben was required.

3. Open a file you are interested in setting breakpoints on the host machine. Find the line and issue `M-x geben-add-current-line-to-predefined-breakpoints`.

4. Check that a breakpoint has been set with `C-h v geben-predefined-breakpoints`

5. Follow process in [web](#web)

6. To set new breakpoints in an active geben buffer, you can use `b` or `M-x geben-set-breakpoint-line`. To set a new breakpoint outside an active geben buffer, open the file on the host system and use `M-x geben-add-current-line-to-predefined-breakpoints` again.

## Perl
[Documentation missing](https://github.com/ahungry/geben/issues/28)

## Python
[Documentation missing](https://github.com/ahungry/geben/issues/28)

## Ruby
[Documentation missing](https://github.com/ahungry/geben/issues/28)

## Tcl
[Documentation missing](https://github.com/ahungry/geben/issues/28)

## Node.js
Even though Node.js ha(s|d) some dbgp protocol support via the [komodo-debug](https://www.npmjs.com/package/komodo-debug) that extension seems to be unmainted and succeded by the newer debug implementations from 7.x forward and geben is not supporting it.
We recommend [jade](https://github.com/NicolasPetton/jade) to debug node applications with Emacs.


# Usage
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
     - C-c b  display breakpoint list
     - >       set redirection mode
     - C-u t  change redirection mode
     - d       display backtrace
     - t       display backtrace
     - v       display context variables
     - C-c f  visit script file
     - w       where
     - q       stop
```

   When you hit any unbound key of `geben-mode`, Geben will ask you to
   edit the original script file. Say yes and Geben will attempts to
   load the script file via `TRAMP`.

   To quit Geben: `M-x geben-end`

# Known Issues

## PHP

- Xdebug does not support STDERR command feature so that STDERR
    redirection feature does not work expectedly.

- Xdebug does not implement `dbgp:` scheme feature so that with
    `step-in` command into a lambda function (you can create it with
    `create_function` in PHP) or mocked function the cursor position is located at
    an invalid line. Geben could handle this more gracefully.

- Xdebug may tell invalid line number on breaking by `return` type
    breakpoint. To this case Geben indicates the cursor at the top of
    the file in where the current breakpoint exists.

- Xdebug unexpectedly breaks on returning from class/instance method
    if there is a `call` type breakpoint to the method.

- Conditional breakpoints currently cannot be deleted

# Contributing
We always need your support - bug reports, feature requests,
and code/documents/design contributions.

To submit one or more of them, please file an issue here or email
Matthew Carter <m@ahungry.com>.

# License
GPLv3
