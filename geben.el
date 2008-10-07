;;; geben.el --- PHP source code level debugger
;; 
;; Filename: geben.el
;; Author: reedom <reedom_@users.sourceforge.net>
;; Maintainer: reedom <reedom_@users.sourceforge.net>
;; Version: 0.11
;; URL: http://sourceforge.net/projects/geben/
;; Keywords: DBGp, debugger, php, Xdebug, python, Komodo
;; Compatibility: Emacs 21.4
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary:
;;
;; This file is part of GEBEN.
;; GEBEN is a PHP source code level debugger.
;; This file contains GEBEN's entry command `geben' and some
;; customizable variables.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Requirements:
;;
;; [Server side]
;; - PHP with Xdebug 2.0.3
;;    http://xdebug.org/
;;
;; [Client side]
;; - Emacs 21.4 and later / XEmacs 21.4 and later having gud package
;; - DBGp client(Debug client)
;;    http://xdebug.org/
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:

(eval-and-compile
  (require 'cl)
  (require 'gud)
  (require 'xml))

;; For compatibility between versions of custom
(eval-and-compile
(condition-case ()
    (require 'custom)
  (error nil))
(if (and (featurep 'custom) (fboundp 'custom-declare-variable)
	 ;; Some XEmacsen w/ custom don't have :set keyword.
	 ;; This protects them against custom.
	 (fboundp 'custom-initialize-set))
    nil ;; We've got what we needed
  ;; We have the old custom-library, hack around it!
  (if (boundp 'defgroup)
      nil
    (defmacro defgroup (&rest args)
      nil))
  (if (boundp 'defcustom)
      nil
    (defmacro defcustom (var value doc &rest args)
      `(defvar (,var) (,value) (,doc))))))

(defgroup geben nil
"A PHP Debugging environment."
:group 'debug)

(defgroup geben-highlighting-faces nil
"Faces for GEBEN."
:group 'geben
:group 'font-lock-highlighting-faces)

(defcustom geben-session-starting-hook nil
"*Hook running at when the geben debugging session is starting."
:group 'geben
:type 'hook)

(defcustom geben-session-finished-hook nil
"*Hook running at when the geben debugging session is finished."
:group 'geben
:type 'hook)

(defcustom geben-after-visit-hook nil
"*Hook running at when..."
:group 'geben
:type 'hook)

(defcustom geben-temporary-file-directory temporary-file-directory
"*Base directory path where GEBEN creates a temporary directory."
:group 'geben
:type 'directory)

(defcustom geben-close-remote-file-after-finish t
"*Specify whether GEBEN should close fetched files from remote site after debugging.
Since the remote files is stored temporary that you can confuse
they were editable if they were left after a debugging session.
If the value is non-nil, GEBEN closes temporary files when
debugging is finished.
If the value is nil, the files left in buffers."
:group 'geben
:type 'boolean)

(defcustom geben-show-breakpoints-debugging-only t
"*Specify breakpoint markers visibility.
If the value is nil, GEBEN will always display breakpoint markers.
If non-nil, displays the markers while debugging but hides after
debugging is finished."
:group 'geben
:type 'boolean)

(defcustom geben-debug-target-remotep nil
"*Specifies whether the debug target is in remote server or local."
:group 'geben
:type 'boolean)

(defface geben-breakpoint-face
'((((class color) (min-colors 88) (background light))
   ;; The background must not be too dark, for that means
   ;; the character is hard to see when the cursor is there.
   (:background "magenta3" :foreground "lightskyblue1"))
  (((class color) (min-colors 88) (background dark))
   (:background "palevioletred2" :foreground "brown4"))
  (((class color) (min-colors 16))
   (:background "magenta4" :foreground "cyan1"))
  (((class color) (min-colors 8))
   (:background "magenta4" :foreground "cyan1"))
  (t (:inverse-video t)))
"Face used to highlight various names.
This includes element and attribute names, processing
instruction targets and the CDATA keyword in a CDATA section.
This is not used directly, but only via inheritance by other faces."
:group 'nxml-highlighting-faces)

;;; #autoload
(defun geben ()
"Start GEBEN, a PHP source level debugger.
GEBEN communicates with script servers, located anywhere local or
remote, which talks DBGp protocol (e.g. PHP with Xdebug extention)
to help you debugging your script with somevaluable features:
 - continuation commands like \`step in\', \`step out\', ...
 - a kind of breakpoints like \`line no\', \`function call\' and
   \`function return\'.
 - evaluation
 - stack dump
 - etc.

The script servers should be DBGp protocol enabled and prepared
to work with the machine running GEBEN. Ask to your script server
administrator about this setting up issue.

The variable `geben-dbgp-command-line' is a command line to
execute a DBGp protocol client command. GEBEN communicates with
script servers through this command.

Once you've done these setup operation correctly, run GEBEN then
run your script on your script server. After some negotiation
GEBEN will display your script's entry source code. So you can
start debugging.
In the debugging session the source code buffers are under the
minor mode  `geben-mode'. Key mapping and other information is
described its help page."
(interactive)
(add-hook 'geben-dbgp-process-hook #'geben-dbgp-entry)
(geben-dbgp))

;;-------------------------------------------------------------
;;  geben-mode
;;-------------------------------------------------------------

(defvar geben-mode-map nil)
(unless geben-mode-map
(setq geben-mode-map (make-sparse-keymap "geben"))
;; control
(define-key geben-mode-map " " 'geben-step-again)
(define-key geben-mode-map "g" 'geben-run)
;;(define-key geben-mode-map "G" 'geben-Go-nonstop-mode)
;;(define-key geben-mode-map "t" 'geben-trace-mode)
;;(define-key geben-mode-map "T" 'geben-Trace-fast-mode)
;;(define-key geben-mode-map "c" 'geben-continue-mode)
;;(define-key geben-mode-map "C" 'geben-Continue-fast-mode)

;;(define-key geben-mode-map "f" 'geben-forward) not implemented
;;(define-key geben-mode-map "f" 'geben-forward-sexp)
;;(define-key geben-mode-map "h" 'geben-goto-here)

;;(define-key geben-mode-map "I" 'geben-instrument-callee)
(define-key geben-mode-map "i" 'geben-step-into)
(define-key geben-mode-map "o" 'geben-step-over)
(define-key geben-mode-map "r" 'geben-step-out)

;; quitting and stopping
(define-key geben-mode-map "q" 'geben-stop)
;;(define-key geben-mode-map "Q" 'geben-top-level-nonstop)
;;(define-key geben-mode-map "a" 'abort-recursive-edit)
(define-key geben-mode-map "S" 'geben-stop)

;; breakpoints
(define-key geben-mode-map "b" 'geben-set-breakpoint)
(define-key geben-mode-map "u" 'geben-unset-breakpoint)
;;(define-key geben-mode-map "B" 'geben-next-breakpoint)
;;(define-key geben-mode-map "x" 'geben-set-conditional-breakpoint)
;;(define-key geben-mode-map "X" 'geben-set-global-break-condition)

;; evaluation
(define-key geben-mode-map "e" 'geben-eval-expression)
;;(define-key geben-mode-map "\C-x\C-e" 'geben-eval-last-sexp)
;;(define-key geben-mode-map "E" 'geben-visit-eval-list)

;; views
;;(define-key geben-mode-map "w" 'geben-where)
;;(define-key geben-mode-map "v" 'geben-view-outside) ;; maybe obsolete??
;;(define-key geben-mode-map "p" 'geben-bounce-point)
;;(define-key geben-mode-map "P" 'geben-view-outside) ;; same as v
;;(define-key geben-mode-map "W" 'geben-toggle-save-windows)

;; misc
;;(define-key geben-mode-map "?" 'geben-help)
;;(define-key geben-mode-map "d" 'geben-backtrace)

;;(define-key geben-mode-map "-" 'negative-argument)

;; statistics
;;(define-key geben-mode-map "=" 'geben-temp-display-freq-count)

;; GUD bindings
(define-key geben-mode-map "\C-c\C-s" 'geben-step-into)
(define-key geben-mode-map "\C-c\C-n" 'geben-step-over)
(define-key geben-mode-map "\C-c\C-c" 'geben-run)

(define-key geben-mode-map "\C-x " 'geben-set-breakpoint)
(define-key geben-mode-map "\C-c\C-d" 'geben-unset-breakpoint)
(define-key geben-mode-map "\C-c\C-t"
  (function (lambda () (geben-set-breakpoint))))
(define-key geben-mode-map "\C-c\C-l" 'geben-where))

(define-minor-mode geben-mode
"Minor mode for debugging source code with GEBEN.
The geben-mode buffer commands:
\\{geben-mode-map}"
nil " *debugging*" geben-mode-map
(setq buffer-read-only geben-mode))
  
(add-hook 'kill-emacs-hook
(lambda ()
  (geben-dbgp-reset)))

(add-hook 'geben-after-visit-hook
(lambda (buf)
  (with-current-buffer buf
    (or (not (fboundp 'geben-mode))
	geben-mode
	(geben-mode t)))))

(defvar geben-step-type :step-into
"Step command of what \`geben-step-again\' acts.
This value remains the latest step command, overwritten at run-time.
So that `geben-step-again'(\\[geben-step-again]) will perform the
same kind of step command.
Value can be one of followings:
 \`:step-into'
 \`:step-out'")

(defun geben-step-again ()
"Do either `geben-step-into' or `geben-step-over' what the last time called.
Default is `geben-step-into'."
(interactive)
(case geben-step-type
  (:step-over (geben-step-over))
  (:step-into (geben-step-into))
  (t (geben-step-into))))
     
(defun geben-step-into ()
"Step into the definition of the function or method about to be called.
If there is a function call involved it will break on the first
statement in that function"
(interactive)
(setq geben-step-type :step-into)
(geben-dbgp-command-step-into))

(defun geben-step-over ()
"Step over the definition of the function or method about to be called.
If there is a function call on the line from which the command
is issued then the debugger engine will stop at the statement
after the function call in the same scope as from where the
command was issued"
(interactive)
(setq geben-step-type :step-over)
(geben-dbgp-command-step-over))

(defun geben-step-out ()
"Step out of the current scope.
It breaks on the statement after returning from the current
function."
(interactive)
(geben-dbgp-command-step-out))

(defun geben-run ()
"Start or resumes the script.
It will break at next breakpoint, or stops at the end of the script."
(interactive)
(geben-dbgp-command-run))

(defun geben-stop ()
"End execution of the script immediately."
(interactive)
(geben-dbgp-command-stop))

(defun geben-set-breakpoint ()
"Set the breakpoint of the current line."
(interactive)
(geben-dbgp-command-breakpoint-set))

(defun geben-unset-breakpoint ()
"Clear the breakpoint of the current line."
(interactive)
(geben-dbgp-command-breakpoint-remove))

(defvar geben-eval-history nil)

(defun geben-eval-expression (expr)
"Evaluate a given string EXPR within the current execution context."
(interactive
 (progn
   (list (read-from-minibuffer "Eval: "
			       nil nil nil 'geben-eval-history))))
(geben-dbgp-command-eval expr))

(defun geben-open-file (fileuri)
"Open a debugger server side file specified by FILEURI.
FILEURI forms like as \`file:///path/to/file\'."
(interactive "s")
(geben-dbgp-command-source fileuri))

;;-------------------------------------------------------------
;;  cross emacs overlay definitions
;;-------------------------------------------------------------

(eval-and-compile
(if (featurep 'xemacs)
    (progn
      (defalias 'geben-overlay-livep 'extent-live-p)
      (defalias 'geben-overlay-make
	(lambda (beg end &optional buffer front-advance rear-advance)
	  (let ((e (make-extent beg end buffer)))
	    (and front-advance
		 (set-extent-property e 'start-open t))
	    (and rear-advance e 'end-open t)
	    e)))
      (defalias 'geben-overlay-move 'set-extent-endpoints)
      (defalias 'geben-overlay-put 'set-extent-property)
      (defalias 'geben-overlay-get 'extent-property)
      (defalias 'geben-overlay-delete 'delete-extent)
      (defalias 'geben-overlays-at
	(lambda (pos) (extent-list nil pos pos)))
      (defalias 'geben-overlays-in 
	(lambda (beg end) (extent-list nil beg end)))
      (defalias 'geben-overlay-buffer 'extent-buffer)
      (defalias 'geben-overlay-start 'extent-start-position)
      (defalias 'geben-overlay-end 'extent-end-position)
      (defalias 'geben-overlay-next-change 'next-extent-change)
      (defalias 'geben-overlay-previous-change 'previous-extent-change)
      (defalias 'geben-overlay-lists
	(lambda () (list (extent-list))))
      (defalias 'geben-overlayp 'extentp)
      )
  (defalias 'geben-overlay-livep 'overlay-buffer)
  (defalias 'geben-overlay-make 'make-overlay)
  (defalias 'geben-overlay-move 'move-overlay)
  (defalias 'geben-overlay-put 'overlay-put)
  (defalias 'geben-overlay-get 'overlay-get)
  (defalias 'geben-overlay-delete 'delete-overlay)
  (defalias 'geben-overlays-at 'overlays-at)
  (defalias 'geben-overlays-in 'overlays-in)
  (defalias 'geben-overlay-buffer 'overlay-buffer)
  (defalias 'geben-overlay-start 'overlay-start)
  (defalias 'geben-overlay-end 'overlay-end)
  (defalias 'geben-overlay-next-change 'next-overlay-change)
  (defalias 'geben-overlay-previous-change 'previous-overlay-change)
  (defalias 'geben-overlay-lists 'overlay-lists)
  (defalias 'geben-overlayp 'overlayp)
  ))

;;-------------------------------------------------------------
;;  DBGp handlers
;;-------------------------------------------------------------

;; -- [dbgp features] --

(defcustom geben-dbgp-feature-alist
'(("max_data" . 65535)
  ("max_depth" . 64))
"*Specifies set of feature variables for each new debugging session."
:group 'geben
:type '(alist :key-type string :key-type sexp))

(defun geben-dbgp-set-features ()
"Configure debugger engine with value of `geben-dbgp-feature-alist'."
(mapc (lambda (cons)
	(geben-dbgp-command-feature-get (car cons))
	(geben-dbgp-command-feature-set (car cons) (cdr cons)))
      geben-dbgp-feature-alist))

;; -- [tid] --

(defvar geben-dbgp-tid 30000
"Transaction ID.")

(defun geben-dbgp-next-tid ()
"Make a new transaction id."
(number-to-string (incf geben-dbgp-tid)))

(defvar geben-dbgp-init-info nil
"Store dbgp initial message.")

;; -- [cmd hash] --

(defvar geben-dbgp-cmd-hash (make-hash-table :test #'equal)
"Hash table of transaction commands.
Key is transaction id used in a dbgp command.
Value is a cmd object.")

(defmacro geben-dbgp-cmd-store (tid cmd)
"Store a CMD to the command transaction list.

TID is transaction id used in a dbgp command.
CMD is a list of command and parameters.
The stored CMD will be pulled later when GEBEN receives a response
message for the CMD."
`(puthash ,tid ,cmd geben-dbgp-cmd-hash))

(defmacro geben-dbgp-cmd-get (tid)
"Get a command object from the command hash table specified by TID."
`(gethash ,tid geben-dbgp-cmd-hash))

(defun geben-dbgp-cmd-remove (tid msg err)
"Remove command from the command hash table."
(let ((cmd (geben-dbgp-cmd-get tid)))
  (remhash tid geben-dbgp-cmd-hash)
  (mapc (lambda (callback)
	  (funcall callback cmd msg err))
	(plist-get cmd :callback))
  cmd))

(defmacro geben-dbgp-cmd-make (operand params &rest callback)
"Create a new command object.
A command object forms a property list with three properties
:operand, :params and :callback."
`(list :operand ,operand :param ,params :callback ,callback))

(defmacro geben-dbgp-cmd-param-arg (cmd flag)
"Get an argument of FLAG from CMD.
For a DBGp command \`stack_get -i 1 -d 2\',
`(geben-dbgp-cmd-param-arg cmd \"-d\")\' gets \"2\"."
`(cdr-safe (assoc ,flag (plist-get ,cmd :param))))

(defun geben-dbgp-cmd-expand (tid cmd)
"Build a send command string for DBGp protocol."
(mapconcat #'(lambda (x)
	       (cond ((stringp x) x)
		     ((integerp x) (int-to-string x))
		     ((atom (format "%S" x)))
		     ((null x) "")
		     (t x)))
	   (geben-flatten (list (plist-get cmd :operand)
				"-i"
				tid
				(plist-get cmd :param)))
	   " "))
  
(defmacro geben-dbgp-cmd-add-callback (cmd &rest callback)
"Add CALLBACK(s) to CMD.
Command callbacks is invoked at when command is finished."
`(dolist (cb (list ,@callback))
   (plist-put ,cmd :callback (cons cb (plist-get cmd :callback)))))

(defmacro geben-dbgp-cmd-sequence (send-command &rest callback)
"Invoke expression sequentially.

CALLBACK is invoked after the response message for SEND-COMMAND
has been received, with three argument. The first one is
SEND-COMMAND. The second is a response message. The third is
decoded error message or nil."
`(let (tid cmd)
   (when (and (setq tid ,send-command)
	      (setq cmd (gethash tid geben-dbgp-cmd-hash)))
     (geben-dbgp-cmd-add-callback cmd ,@callback))))

;; -- [code hash] --

(defvar geben-dbgp-source-hash (make-hash-table :test #'equal)
"Hash table of source files.
Key is a fileuri to a source file being debugged.
Value is a cons of (remotep . filepath).")

(defmacro geben-dbgp-source-make (fileuri remotep local-path)
"Create a new source object.
A source object forms a property list with three properties
:fileuri, :remotep and :local-path."
`(list :fileuri ,fileuri :remotep ,remotep :local-path ,local-path))

(defun geben-dbgp-cleanup-file (source)
(let ((buf (find-buffer-visiting (plist-get source :local-path))))
  (when buf
    (with-current-buffer buf
      (when geben-mode
	(geben-mode nil))
      ;;	  Not implemented yet
      ;; 	  (and (buffer-modified-p buf)
      ;; 	       (switch-to-buffer buf)
      ;; 	       (yes-or-no-p "Buffer is modified. Save it?")
      ;; 	       (geben-write-file-contents this buf))
      (when (and geben-close-remote-file-after-finish
		 (plist-get source :remotep))
	(set-buffer-modified-p nil)
	(kill-buffer buf))))))

(defvar geben-dbgp-stack nil
"A stack list.")

;; -- [breakpoints] --

(defvar geben-dbgp-breakpoints nil
"A break point list")

(defun geben-dbgp-bp-lineno= (lhs rhs)
(and (eq (plist-get lhs :type) :lineno)
     (eq (plist-get lhs :type)
	 (plist-get rhs :type))
     (string= (plist-get lhs :fileuri)
	      (plist-get rhs :fileuri))
     (eq (plist-get lhs :lineno)
	 (plist-get rhs :lineno))))

(defun geben-overlay-make-line (lineno &optional buf)
(with-current-buffer (or buf (current-buffer))
  (save-excursion
    (widen)
    (message "%s - %s" (point-min) (point-max))
    (goto-line lineno)
    (beginning-of-line)
    (geben-overlay-make (point)
			(save-excursion
			  (forward-line) (point))
			nil t nil))))

(defun geben-dbgp-bp-lineno-make (fileuri lineno &optional local-path id overlay)
(let ((bp (list :type :lineno
		:fileuri fileuri
		:lineno lineno
		:local-path (or local-path "")
		:id (or id "")
		:overlay overlay)))
  (unless overlay
    (geben-dbgp-bp-lineno-setup-overlay bp))
  bp))

(defun geben-dbgp-bp-lineno-setup-overlay (bp)
(geben-dbgp-bp-finalize bp)
(let* ((local-path (plist-get bp :local-path))
       (overlay (and (stringp local-path)
		     (find-buffer-visiting local-path)
		     (geben-overlay-make-line (plist-get bp :lineno)
					      (find-buffer-visiting local-path)))))
  (when overlay
    (geben-overlay-put overlay 'face 'geben-breakpoint-face)
    (geben-overlay-put overlay 'evaporate t)
    (geben-overlay-put overlay 'bp bp)
    (geben-overlay-put overlay 'modification-hooks '(geben-dbgp-bp-overlay-modified))
    (geben-overlay-put overlay 'insert-in-front-hooks '(geben-dbgp-bp-overlay-inserted-in-front))
    (plist-put bp :overlay overlay)))
bp)

(defun geben-dbgp-bp-overlay-modified (overlay afterp beg end &optional len)
(message (format "mod: %s %d - %d(%S)" (if afterp "after" "before") beg end len))
(when afterp
  (save-excursion
    (save-restriction
      (widen)
      (let* ((lineno-from (progn (goto-char (geben-overlay-start overlay))
				 (geben-what-line)))
	     (lineno-to (progn (goto-char (geben-overlay-end overlay))
			       (geben-what-line)))
	     (lineno lineno-from))
	(goto-line lineno)
	(while (and (looking-at "[ \t]*$")
		    (< lineno lineno-to))
	  (forward-line)
	  (incf lineno))
	(if (< lineno-from lineno)
	    (plist-put (geben-overlay-get overlay 'bp) :lineno lineno))
	(goto-line lineno)
	(beginning-of-line)
	(geben-overlay-move overlay (point) (save-excursion
					      (forward-line)
					      (point))))))))

(defun geben-dbgp-bp-overlay-inserted-in-front (overlay afterp beg end &optional len)
(if afterp
    (save-excursion
      (goto-line (progn (goto-char (geben-overlay-start overlay))
			(geben-what-line)))
      (geben-overlay-move overlay (point) (save-excursion
					    (forward-line)
					    (point))))))

(defun geben-dbgp-bp-lineno-find (fileuri lineno)
(let* ((tmpbp (geben-dbgp-bp-lineno-make fileuri lineno))
       (pos (position-if (lambda (bp)
			   (geben-dbgp-bp-lineno= bp tmpbp))
			 geben-dbgp-breakpoints)))
  (when pos
    (nth pos geben-dbgp-breakpoints))))

(defun geben-dbgp-bp-add (bp)
(add-to-list 'geben-dbgp-breakpoints bp t))

(defun geben-dbgp-bp-remove (id)
(setq geben-dbgp-breakpoints
      (remove-if (lambda (bp)
		   (when (string= (plist-get bp :id) id)
		     (geben-dbgp-bp-finalize bp)))
		 geben-dbgp-breakpoints)))

(defun geben-dbgp-bp-finalize (bp)
(and (eq (plist-get bp :type) :lineno)
     (geben-overlayp (plist-get bp :overlay))
     (geben-overlay-delete (plist-get bp :overlay)))
bp)

(defun geben-dbgp-bp-find-file-hook ()
(and geben-dbgp-init-info
     (not geben-show-breakpoints-debugging-only)
     (let ((buf (current-buffer)))
       (mapc (lambda (bp)
	       (and (eq (plist-get bp :type) :lineno)
		    (eq (find-buffer-visiting (plist-get bp :local-path)) buf)
		    (geben-dbgp-bp-lineno-setup-overlay bp)))
	     geben-dbgp-breakpoints))))

(add-hook 'find-file-hooks 'geben-dbgp-bp-find-file-hook)

(defun geben-dbgp-restore-breakpoints ()
"Restore breakpoints against new dbgp session."
(let (overlay)
  (mapc (lambda (bp)
	  (case (plist-get bp :type)
	    (:lineno
	     ;; User may edit code since previous debuggin session
	     ;; so that lineno breakponts set before may moved.
	     ;; The followings try to adjust breakpoint line to
	     ;; nearly what user expect.
	     (if (and (setq overlay (plist-get bp :overlay))
		      (geben-overlayp overlay)
		      (geben-overlay-livep overlay)
		      (eq (geben-overlay-buffer overlay)
			  (find-buffer-visiting (plist-get bp :local-path))))
		 (with-current-buffer (geben-overlay-buffer overlay)
		   (save-excursion
		     (plist-put bp :lineno (progn (goto-char (geben-overlay-start overlay))
						  (geben-what-line))))))
	       
	     (geben-dbgp-command-breakpoint-set t
						(plist-get bp :fileuri)
						(plist-get bp :lineno)
						(plist-get bp :local-path)))))
	geben-dbgp-breakpoints)))

(defun geben-dbgp-bp-hide-breakpoints ()
(mapc (lambda (bp)
	(case (plist-get bp :type)
	  (:lineno
	   (let ((overlay (plist-get bp :overlay)))
	     (and (geben-overlayp overlay)
		  (geben-overlay-livep overlay)
		  (geben-overlay-put overlay 'face nil))))))
      geben-dbgp-breakpoints))
  
(defun geben-session-init-variables ()
(setq geben-dbgp-stack nil
      geben-dbgp-init-info nil)
(clrhash geben-dbgp-cmd-hash)
(clrhash geben-dbgp-source-hash))
  
(defun geben-dbgp-reset ()
(setq gud-last-frame nil)
(setq gud-overlay-arrow-position nil)
(maphash (lambda (fileuri source)
	   (geben-dbgp-cleanup-file source))
	 geben-dbgp-source-hash)
(when geben-show-breakpoints-debugging-only
  (geben-dbgp-bp-hide-breakpoints))
(geben-session-init-variables)
(ignore-errors
  (geben-delete-directory-tree (geben-temp-dir))))

;;; dbgp protocol handler

(defun geben-dbgp-entry (msg)
"Analyze MSG and dispatch to a specific handler."
(case (xml-node-name msg)
  ('connect
   t)
  ('init
   (setq geben-dbgp-init-info msg)
   (geben-dbgp-set-features)
   (geben-dbgp-restore-breakpoints)
   (geben-dbgp-prepare-source-file (xml-get-attribute msg 'fileuri))
   (geben-dbgp-command-step-into))
  ('response
   (geben-dbgp-handle-response msg))
  ('otherwise
   ;;mada
   (message "unknown protocol: %S" msg))))

(defmacro geben-dbgp-tid-of (xml)
`(cdr (assoc 'transaction_id (cadr ,xml))))
  
(defun geben-dbgp-handle-response (msg)
"Handle a response meesage."
(let* ((tid (geben-dbgp-tid-of msg))
       (cmd (geben-dbgp-cmd-get tid))
       (err (ignore-errors (xml-get-children msg 'error))))
  (if err
      (message "Command error: %s"
	       (third (car-safe (xml-get-children (car err) 'message))))
    (let* ((operand (replace-regexp-in-string
		     "_" "-" (xml-get-attribute msg 'command)))
	   (func-name (concat "geben-dbgp-response-" operand))
	   (func (intern-soft func-name)))
      (if (and cmd (functionp func))
	  (funcall func cmd msg)
	(message "%s is not defined" func-name))))
  (geben-dbgp-cmd-remove tid msg err))
(geben-dbgp-handle-status msg))

(defun geben-dbgp-handle-status (msg)
"Handle status code in a response message."
(let ((status (xml-get-attribute msg 'status)))
  (cond
   ((equal status "stopping")
    (geben-dbgp-command-stop))
   ((equal status "stopped")
    (gud-basic-call "")
    (geben-dbgp-reset)
    (run-hooks 'geben-session-finished-hook)
    (message "xdebug debugging finished!"))
   ((equal status "break")
    (geben-dbgp-command-stack-get)))))

;;; command sending

(defun geben-send-raw-command (fmt &rest arg)
"Send a command string to a debugger engine.

The command string will be built up with FMT and ARG with a help of
the string formatter function `fomrat'."
(let ((cmd (apply #'format fmt arg)))
  (gud-basic-call cmd)))

(defun geben-dbgp-send-command (operand &rest params)
"Send a command to a debugger engine.

This function automatically inserts a transaction ID which is
required for each dbgp command by the protocol specification."
(let ((cmd (geben-dbgp-cmd-make operand params))
      (tid (geben-dbgp-next-tid)))
  (geben-dbgp-cmd-store tid cmd)
  (gud-call (geben-dbgp-cmd-expand tid cmd))
  tid))

;;;
;;; command/response handlers
;;;

;; step_into

(defun geben-dbgp-command-step-into ()
"Send \`step_into\' command."
(geben-dbgp-send-command "step_into"))

(defun geben-dbgp-response-step-into (cmd msg)
"A response message handler for a \`step_into\' command."
nil)

;; step_over

(defun geben-dbgp-command-step-over ()
"Send \`step_over\' command."
(geben-dbgp-send-command "step_over"))

(defun geben-dbgp-response-step-over (cmd msg)
"A response message handler for a \`step_over\' command."
nil)

;; step_out
(defun geben-dbgp-response-step-out (cmd msg)
"A response message handler for a \`step_out\' command."
nil)

(defun geben-dbgp-command-step-out ()
"Send \`step_out\' command."
(geben-dbgp-send-command "step_out"))

;; run

(defun geben-dbgp-command-run ()
"Send \`run\' command."
(geben-dbgp-send-command "run"))

(defun geben-dbgp-response-run (cmd msg)
"A response message handler for a \`run\' command."
nil)

;;; stop

(defun geben-dbgp-command-stop ()
"Send \`stop\' command."
(geben-dbgp-send-command "stop"))

(defun geben-dbgp-response-stop (cmd msg)
"A response message handler for a \`stop\' command."
nil)

;;; breakpoint_set

(defun geben-dbgp-command-breakpoint-set (&optional force fileuri lineno path)
"Send \`breakpoint_set\' command."
(setq path (or path
	       (buffer-file-name (current-buffer))))
(when path
  (setq lineno (or lineno
		   (and (get-file-buffer path)
			(with-current-buffer (get-file-buffer path)
			  (geben-what-line)))))
  (setq fileuri (or fileuri
		    (geben-dbgp-find-fileuri path)
		    (concat "file://" (file-truename path))))
  (when (or force
	    (null (geben-dbgp-bp-lineno-find fileuri lineno)))
    (geben-dbgp-send-command
     "breakpoint_set"
     (cons "-t" "line")
     (cons "-f" fileuri)
     (cons "-n" lineno)))))

(defun geben-dbgp-response-breakpoint-set (cmd msg)
"A response message handler for a \`breakpoint_set\' command."
(let ((type (geben-dbgp-cmd-param-arg cmd "-t"))
      (id (xml-get-attribute msg 'id)))
  (cond
   ((equal type "line")
    (let* ((fileuri (geben-dbgp-cmd-param-arg cmd "-f"))
	   (lineno (geben-dbgp-cmd-param-arg cmd "-n"))
	   (path (or (geben-dbgp-get-local-path-of fileuri)
		     (geben-temp-path-for-fileuri fileuri)))
	   (bp (geben-dbgp-bp-lineno-find fileuri lineno)))
      (when bp
	(geben-dbgp-bp-remove (plist-get bp :id)))
      (geben-dbgp-bp-add
       (geben-dbgp-bp-lineno-make fileuri lineno path id)))))))

;;; breakpoint_remove

(defun geben-dbgp-command-breakpoint-remove (&optional fileuri path lineno)
"Send \`breakpoint_remove\' command."
(setq path (or path
	       (buffer-file-name (current-buffer))))
(when path
  (setq lineno (or lineno
		   (and (get-file-buffer path)
			(with-current-buffer (get-file-buffer path)
			  (geben-what-line)))))
  (setq fileuri (or fileuri
		    (geben-dbgp-find-fileuri path)
		    (concat "file://" (file-truename path))))
  (when (and fileuri lineno)
    (let* ((bp (geben-dbgp-bp-lineno-find fileuri lineno))
	   (bid (and bp (plist-get bp :id))))
      (when bp
	(geben-dbgp-cmd-sequence
	 (geben-dbgp-send-command "breakpoint_remove" (cons "-d" bid))
	 `(lambda (cmd msg err)
	    (when err
	      ;; it should a stray breakpoint; remove it from bp hash table.
	      (geben-dbgp-bp-remove ,bid)))))))))

(defun geben-dbgp-response-breakpoint-remove (cmd msg)
"A response message handler for a \`breakpoint_remove\' command."
(let* ((bp (car-safe (xml-get-children msg 'breakpoint)))
       (id (xml-get-attribute bp 'id)))
  (geben-dbgp-bp-remove id)))

;;; stack_get

(defun geben-dbgp-command-stack-get ()
"Send \`stack_get\' command."
(geben-dbgp-send-command "stack_get"))

(defun geben-dbgp-response-stack-get (cmd msg)
"A response message handler for a \`stack_get\' command."
(let* ((stack (car-safe (xml-get-children msg 'stack)))
       (fileuri (xml-get-attribute stack 'filename))
       (lineno (xml-get-attribute stack'lineno))
       local-path)
  (when (and fileuri lineno)
    (if (setq local-path (geben-dbgp-get-local-path-of fileuri t))
	(geben-dbgp-indicate-current-line local-path lineno)
      (geben-dbgp-cmd-sequence
       (geben-dbgp-command-source fileuri)
       `(lambda (cmd msg err)
	  (when (not err)
	    (geben-dbgp-indicate-current-line
	     (geben-dbgp-get-local-path-of ,fileuri) ,lineno)
	    (gud-display-frame))))))))

;;; eval

(defun geben-dbgp-command-eval (exp)
"Send \`eval\' command."
(geben-dbgp-send-command
 "eval"
 (format "-- {%s}" (base64-encode-string exp))))

(defun geben-dbgp-response-eval (cmd msg)
"A response message handler for a \`eval\' command."
(message "result: %S" 
	 (geben-dbgp-decode-value (car-safe (xml-get-children msg 'property)))))

(defun geben-dbgp-decode-value (prop)
"Decode a VALUE passed by xdebug engine."
(let ((type (xml-get-attribute prop 'type))
      result)
  (setq result
	(cond
	 ((or (string= "array" type)
	      (string= "object" type))
	  (mapcar (lambda (value)
		    (geben-dbgp-decode-value value))
		  (xml-get-children prop 'property)))
	 ((string= "null" type)
	  nil)
	 (t
	  (let ((value (car (last prop))))
	    (assert (stringp value))
	    (when (string= "base64" (xml-get-attribute prop 'encoding))
	      (setq value (base64-decode-string value)))
	    (if (string= "string" type)
		(decode-coding-string value 'utf-8)
	      (string-to-number value))))))
  (let ((name (xml-get-attribute prop 'name)))
    (if (string< "" name)
	(cons name result)
      result))))
	   
;;; source

(defun geben-dbgp-command-source (fileuri)
"Send source command.
FILEURI is a uri of the target file of a debuggee site."
(geben-dbgp-send-command "source" (cons "-f" fileuri)))

(defun geben-dbgp-response-source (cmd msg)
"A response message handler for a \`source\' command."
(let* ((fileuri (geben-dbgp-cmd-param-arg cmd "-f"))
       ;; (decode-coding-string (base64-decode-string (third msg)) 'undecided)))))
       (path (geben-temp-path-for-fileuri fileuri)))
  (when path
    (geben-temp-store path (base64-decode-string (third msg))))
  (puthash fileuri (geben-dbgp-source-make fileuri t path) geben-dbgp-source-hash)
  (geben-visit-file path)))

(defun geben-dbgp-command-feature-get (feature)
"Send \`feature_get\' command."
(geben-dbgp-send-command "feature_get" (cons "-n" feature)))

(defun geben-dbgp-response-feature-get (cmd msg)
"A response message handler for a \`feature_get\' command."
(and t nil))

(defun geben-dbgp-command-feature-set (feature value)
"Send \`feature_get\' command."
(geben-dbgp-send-command "feature_set"
			 (cons "-n" feature)
			 (cons "-v" (format "%S" (eval value)))))

(defun geben-dbgp-response-feature-set (cmd msg)
"A response message handler for a \`feature_get\' command."
(and t nil))

;;;

(defun geben-dbgp-prepare-source-file (fileuri)
"Prepare source file to be in the local machine.
If the counter-file of FILEURI is already known by the current
debugging session, do nothing.  
If `geben-debug-target-remotep' is non-nil or not exists locally, fetch
the file from remote site using \`source\' command then stores in
a GEBEN's temporal direcotory tree."
(unless (geben-dbgp-get-local-path-of fileuri)
  (let ((local-path (geben-make-local-path fileuri)))
    (if (or geben-debug-target-remotep
	    (not (file-exists-p local-path)))
	;; haven't fetched remote source yet; fetch it.
	(geben-dbgp-command-source fileuri)
      ;; don't know why but the temporal copy of the remote's source exists.
      (let ((source (geben-dbgp-source-make fileuri t local-path)))
	(puthash fileuri source geben-dbgp-source-hash)
	(geben-visit-file (plist-get source :local-path)))))))

(defun geben-dbgp-find-fileuri (path)
"Find fileuri for PATH."
(let (fileuri)
  (maphash (lambda (key source)
	     (when (string= (plist-get source :local-path) path)
	       ;; todo: how can I stop this iteration?
	       (setq fileuri key)))
	   geben-dbgp-source-hash)
  fileuri))
	     
(defun geben-dbgp-get-local-path-of (fileuri &optional markp)
  (let ((source (gethash fileuri geben-dbgp-source-hash)))
    (if source
	(plist-get source :local-path)
      ;; not konwn for the current session.
      (let ((local-path (replace-regexp-in-string "^file://" "" fileuri)))
	(when (and (not geben-debug-target-remotep)
		   (file-exists-p local-path))
	  (when (and markp
		     (not (gethash fileuri geben-dbgp-source-hash)))
	    (puthash fileuri (geben-dbgp-source-make fileuri nil local-path) geben-dbgp-source-hash))
	  local-path)))))

;; -- [gud] --

(defcustom geben-dbgp-command-line "debugclient -p 9000"
  "*Command line string to execute DBGp client."
  :type 'string
  :group 'gud)

(defcustom geben-dbgp-process-hook nil
  "*Hook running at each dbgp protocol message.
Each hook functions is called with one argument XML which is a
XMLized dbgp protocol message."
  :type 'hook
  :group 'geben)

(defun geben-dbgp-process-chunk (xml)
  "Process a DBGp response chunk."
  (run-hook-with-args 'geben-dbgp-process-hook (car-safe xml)))

;; There's no guarantee that Emacs will hand the filter the entire
;; marker at once; it could be broken up across several strings.  We
;; might even receive a big chunk with several markers in it.  If we
;; receive a chunk of text which looks like it might contain the
;; beginning of a marker, we save it here between calls to the filter.
(defun geben-dbgp-marker-filter (string)
  "Process DBGp response STRING.
Parse STRING, find xml chunks, convert them to xmlized lisp objects
and call `geben-dbgp-process-chunk' with each chunk."
  (setq gud-marker-acc (concat gud-marker-acc (delete ?\r string)))
  (let (xml-list
        (output ""))
    (flet ((parse-xml (str)
                      (with-temp-buffer
                        (insert str)
                        (ignore-errors (xml-parse-region (point-min) (point-max)))))
           (xmlize (offset)
                   (when (string-match "<\\?xml" gud-marker-acc offset)
                     (let* ((beg (match-beginning 0))
                            (end (and (string-match "^\\((cmd)\\|<\\?xml\\)" gud-marker-acc (1+ beg))
                                      (match-beginning 0))))
                       (if (null end)
                           beg
                         (let ((xml (parse-xml (substring gud-marker-acc beg end))))
                           (when xml
                             (add-to-list 'xml-list xml t))
                           (xmlize end)))))))
      (setq output
            (let ((acc-pos (xmlize 0)))
              ;; Does the remaining text look like it might end with the
              ;; beginning of another marker?  If it does, then keep it in
              ;; gud-marker-acc until we receive the rest of it.  Since we
              ;; know the full marker regexp above failed, it's pretty simple to
              ;; test for marker starts.
              (if acc-pos
                  (prog1
                      ;; Everything before the potential marker start can be output.
                      (substring gud-marker-acc 0 acc-pos)
                    (setq gud-marker-acc
                          (substring gud-marker-acc acc-pos)))
                ;; Everything after, we save, to combine with later input.
                (prog1
                    gud-marker-acc
                  (setq gud-marker-acc "")))))
      (mapc #'geben-dbgp-process-chunk xml-list))
    output))

(defun geben-dbgp-find-file (path)
  "Visit debuggee file specified by PATH.
After visited it invokes `geben-after-visit-hook'."
  (let ((buffer (or (find-buffer-visiting path)
		    (and (file-exists-p path)
			 (find-file-noselect path)))))
    (when buffer
      (prog1
	  (pop-to-buffer buffer)
	(run-hook-with-args 'geben-after-visit-hook buffer)))))

(defun geben-dbgp-indicate-current-line (local-path lineno)
  "Display current debugging position marker."
  (setq gud-last-frame
	(cons local-path (string-to-number lineno)))
  (message "stopped: %s(%s)"
	   (file-name-nondirectory local-path) lineno))

(defun geben-dbgp (&optional command-line)
  "Run a DBGp client program.
If the optional argument COMMAND-LINE is nil, the value of
`geben-dbgp-command-line' is used."
  (interactive "P")
  (save-window-excursion
    (when (and gud-comint-buffer
	       (buffer-name gud-comint-buffer))
      (kill-buffer gud-comint-buffer))
    (gud-common-init geben-dbgp-command-line nil
		     'geben-dbgp-marker-filter 'geben-dbgp-find-file)
    (with-current-buffer gud-comint-buffer
      (set-process-query-on-exit-flag (get-buffer-process (current-buffer)) nil)
      (add-hook 'kill-buffer-hook 'geben-dbgp-reset nil t))

    (set (make-local-variable 'gud-minor-mode) 'xdebug)
    ;;  (gud-def gud-break  "b %l"         "\C-b" "Set breakpoint at current line.")
    ;;  (gud-def gud-remove "d %l"         "\C-d" "Remove breakpoint at current line")
    ;;  (gud-def gud-step   "s"            "\C-s" "Step one source line with display.")
    ;;  (gud-def gud-next   "n"            "\C-n" "Step one line (skip functions).")
    ;;  (gud-def gud-cont   "c"            "\C-r" "Continue with display.")
    ;;  (gud-def gud-finish "finish"       "\C-f" "Finish executing current function.")
    ;;  (gud-def gud-up     "up %p"        "<" "Up N stack frames (numeric arg).")
    ;;  (gud-def gud-down   "down %p"      ">" "Down N stack frames (numeric arg).")
    ;;  (gud-def gud-print  "%e"           "\C-p" "Evaluate perl expression at point.")
    (setq comint-prompt-regexp "^(cmd) ")
    (setq paragraph-start comint-prompt-regexp)
    (run-hooks 'geben-mode-hook))
  (message "Waiting for debug server to connect."))

;;-------------------------------------------------------------
;;  miscellaneous functions
;;-------------------------------------------------------------

;; -- [temporary directory] --

(defun geben-temp-dir ()
  "Get a temporary directory path for a GEBEN session."
  (let ((base-dir (file-truename (expand-file-name "emacs-geben"
						   geben-temporary-file-directory))))
    (unless (file-exists-p base-dir)
      (make-directory base-dir t)
      (set-file-modes base-dir 1023))
    (expand-file-name (format "%d" (emacs-pid)) base-dir)))

(defun geben-temp-path-for-fileuri (fileuri)
  "Generate path string from FILEURI to store files temporarily."
  (when (string-match "^file:///?" fileuri)
    (expand-file-name (substring fileuri (match-end 0)) (geben-temp-dir))))

(defun geben-temp-store (path source)
  "Store temporary file."
  (make-directory (file-name-directory path) t)
  (ignore-errors
    (with-current-buffer (or (find-buffer-visiting path)
			     (create-file-buffer path))
      (widen)
      (erase-buffer)
      (font-lock-mode 0)
      (let ((encoding (detect-coding-string source t)))
	(unless (eq 'undecided encoding)
	  (set-buffer-file-coding-system encoding))
	(insert (decode-coding-string source encoding)))
      (with-temp-message ""
	(write-file path)
	(kill-buffer (current-buffer))))
    t))

(defun geben-delete-directory-tree (base-path)
  "Delete directory tree."
  (if (file-directory-p base-path)
      (progn
	(mapc (lambda (name)
		(let ((fullpath (expand-file-name name base-path)))
		  (cond
		   ((equal name ".") t)
		   ((equal name "..") t)
		   ((or (file-symlink-p fullpath)
			(file-regular-p fullpath))
		    (delete-file fullpath))
		   ((file-directory-p fullpath)
		    (geben-delete-directory-tree fullpath)))))
	      (directory-files base-path nil nil t))
	(delete-directory base-path))))

;; -- [path]--

(defun geben-make-local-path (fileuri)
  "Make a path string derinved from FILEURI."
  (let ((local-path (replace-regexp-in-string "^file://" "" fileuri)))
    (when (eq system-type 'windows-nt)
      (require 'url-util)
      (setq local-path (url-unhex-string (substring local-path 1))))
    local-path))

;; -- [source code file]--

(defun geben-visit-file (path)
  "Visit to a local source code file."
  (when (file-exists-p path)
    (let ((buf (find-file-noselect path)))
      (pop-to-buffer buf)
      (run-hook-with-args 'geben-after-visit-hook buf)
      buf)))

;; -- [utility]--

(defun geben-flatten (x)
  "Make cons X to a flat list."
  (labels ((rec (x acc)
		(cond ((null x) acc)
		      ((atom x) (cons x acc))
		      (t (rec (car x) (rec (cdr x) acc))))))
    (rec x nil)))

(defun geben-what-line (&optional pos)
  "Get the number of the line in which POS is located.
If POS is ommitted, then the current position is used."
  (let ((opoint (or pos (point))))
    (save-restriction
      (widen)
      (save-excursion
	(forward-line 0)
	(1+ (count-lines 1 opoint))))))


(provide 'geben)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; geben.el ends here
