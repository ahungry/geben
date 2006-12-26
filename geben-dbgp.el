;;; geben-dbgp.el --- 
;; 
;; Filename: geben-dbgp.el
;; Description: 
;; Author: thoney <thoney@nakarika.com>
;; Maintainer: thoney <thoney@nakarika.com>
;; Created: Sat Oct 21 16:10:01 2006
;; Version: 0.01
;; Last-Updated: Mon Dec 25 16:05:03 2006 (32400 JST)
;;           By: thoney <thoney@nakarika.com>
;;     Update #: 144
;; URL: http://sourceforge.net/projects/geben/
;; Keywords: php, debugger
;; Compatibility: 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;; This file is part of GEBEN.
;; This contents is for handling the dbgp protocol.
;; The dbgp protocol is used by Xdebug. The protocol specification
;; is found at http://xdebug.org/docs-dbgp.php.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Note: 
;; 
;; Desctructors - they are called explicitly until eieio finds the
;; solution to call destructors implicitly.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change log:
;; 
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

(require 'eieio)
(require 'geben-session)
(require 'geben-dbgp-cmd)
(require 'geben-source-code)

;;; <geben-dbgp-session> manipulators

(defclass <geben-dbgp-session> (<geben-gud-session>)
  ((remotep :initarg :remote
	    :type boolean
	    :initform nil
	    :documentation
	    "Specifies whether the debug target is remote or local.")
   (tid :initform 0
	:type integer
	:documentation
	"Transaction ID.")
   (init-info :initarg :init-info
	      :type list
	      :documentation
	      "Store init dbgp message.")
   (trans-cmd :initform (lambda () (make-hash-table :test #'equal))
	      :documentasion
	      "Hash table of transaction commands.
Key is transaction id used in a dbgp command.
Value is a <geben-dbgp-command> object.")
   (source-files :initform (lambda () (make-hash-table :test #'equal))
		 :documentasion
		 "Hash table of source files.
Key is fileuri of the source file.
Value is path to localy copied file stored temporarily.")
   (stack :initform nil
	  :documentation
	  "Current stack list."))
  :documentation
  "A dbgp session.")
   
(defclass <geben-dbgp-features> ()
  ((language_supports_threads :type boolean
			      :readonly t)
   (language_name :type string
		  :readonly t)
   (language_version :type string
		     :readonly t)
   (encoding :type string
	     :readonly t
	     :documentation
	     "current encoding in use by the debugger session.")
   (protocol_version :type string
		     :readonly t
		     :documentation
		     "{for now, always 1}")
   (supports_async :type boolean
		   :readonly t
		   :documentation
		   "{for commands such as break}")
   (data_encoding :type string
		  :readonly t
		  :documentation
		  "optional, allows to turn off the default
base64 encoding of data. This should only be used for
development and debugging of the debugger engines themselves,
and not for general use.  If implemented the value 'base64'
must be supported to turn back on regular encoding. the value
'none' means no encoding is in use. all elements that use
encoding must include an encoding attribute.
breakpoint_languages get some engines may support more than
one language. This feature returns a string which is a comma
separated list of supported languages. If the engine does not
provide this feature, then it is assumed that the engine only
supports the language defined in the feature
language_name. One example of this is an XSLT debugger engine
which supports XSLT, XML, HTML and XHTML. An IDE may need this
information to to know what types of breakpoints an engine
will accept.")
   (multiple_sessions :type boolean)
   (encoding :type string
	     :documentation
	     "{ISO8859-15, UTF-8, etc.}")
   (max_children :type integer
		 :documentation
		 "max number of array or object children to
initially retrieve.")
   (max_data :type integer
	     :documentation
	     "max amount of variable data to initially retrieve.")
   (max_depth :type integer
	      :documentation
	      "maximum depth that the debugger engine may return
when sending arrays, hashs or object structures to the IDE.")
   (data_encoding :type string
		  :documentation
		  "see feature_get")
   (supports_postmortem :type boolean
			:readonly t
			:documentation
			"This feature lets an IDE know that there
is benefit to continuing interaction during the STOPPING state")
   (show_hidden :type boolean
		:documentation
		"This feature can get set by the IDE if it wants
to have more detailed internal information on properties (eg.
private members of classes, etc.) Zero means that hidden
members are not shown to the IDE.")
   (notify_ok :type boolean
	      :documentation "See section 8.5.")))

(defmethod destructor ((this <geben-dbgp-session>) &rest params)
  "Destructor for cleaning up any dynamic links to our object.
Argument THIS is the object being destroyed.  PARAMS are additional
ignored parameters."
  (setq gud-last-frame nil)
  ;; close buffer opening temp files.
  (maphash (lambda (fileuri obj)
	     (destructor obj))
	   (oref this source-files))
  ;; remove temp files.
  (ignore-errors
    (geben-delete-directory-tree (geben-temp-dir))))

(defmethod geben-mktid ((this <geben-dbgp-session>))
  "Make a new transaction id."
  (md5 (number-to-string (incf (slot-value this 'tid)))))

(defmethod geben-store-cmd ((this <geben-dbgp-session>) tid cmd)
  "Store a CMD to the command transaction list belongs to THIS

TID is transaction id used in a dbgp command.
CMD is a list of command and parameters.
The stored CMD will be pulled later when GEBEN receives a response
message for the CMD."
  (puthash tid cmd (oref this trans-cmd)))

(defmethod geben-remove-cmd ((this <geben-dbgp-session>) msg)
  "Remove command from the command transaction list."
  (let* ((tid (cdr (assoc 'transaction_id (cadr msg))))
	 (cmd (gethash tid (oref this trans-cmd))))
    (remhash tid (oref this trans-cmd))
    cmd))

;;; dbgp protocol handler

(defun geben-dbgp-entry (msg)
  "Analyze MSG and dispatch to a specific handler."
  (case (xml-node-name msg)
    ('connect
     t)
    ('init
     (geben-dbgp-create-session msg)
     (geben-restore-breakpoints (geben-current-session))
     (geben-setup-source-file (geben-current-session)
			      (xml-get-attribute msg 'fileuri)))
    ('response
     (geben-handle-response (geben-current-session) msg))
    ('otherwise
     ;;mada
     (message "unknown protocol: %S" msg))))

(defun geben-dbgp-create-session (msg)
  "Create a new dbgp session."
  (when geben-current-session
    (destructor geben-current-session))	;call explicitly until eieio
					;find the solution to call
					;destructors implicitly.
  (setq geben-current-session (<geben-dbgp-session> "geben" :init-info msg)))

(defmethod geben-restore-breakpoints ((this <geben-dbgp-session>))
  "Restore breakpoints set for the starting session THIS."
  (mapc (lambda (bp)
	  (case (class-of bp)
	    ('<geben-breakpoint-lineno>
	     (geben-command-breakpoint-set this t
					   (oref bp fileuri)
					   (oref bp path)
					   (oref bp lineno)))))
	geben-breakpoints))

(defmethod geben-handle-response ((this <geben-dbgp-session>) msg)
  "Handle a response meesage."
  (if (condition-case nil
	  (xml-get-children msg 'error)
	(error nil))
      (message "Command error: %s"
	       (third (car-safe
		       (xml-get-children
			(car-safe (xml-get-children msg 'error))
			'message))))
    (let* ((operand (replace-regexp-in-string
		     "_" "-" (xml-get-attribute msg 'command)))
	   (func-name (concat "geben-response-" operand))
	   (func (intern-soft func-name))
	   (cmd (geben-remove-cmd this msg)))
      (if (and cmd (functionp func))
	  (funcall func this cmd msg)
	(message "%s is not defined" func-name))
      (geben-handle-status this msg))))

(defmethod geben-handle-status ((this <geben-dbgp-session>) msg)
  "Handle status code in a response message."
  (let ((status (xml-get-attribute msg 'status)))
    (cond
     ((equal status "stopped")
      (gud-basic-call "")
      (setq gud-last-frame nil)
      (setq overlay-arrow-position nil)
      (geben-finished this)
      (message "xdebug debugging finished!"))
     ((equal status "break")
      (geben-command-stack-trace this)))))

;;; command sending

(defmethod geben-send-raw-command ((this <geben-gud-session>) fmt &rest arg)
  "Send a command string to a debugger engine.

The command string will be built up with FMT and ARG with a help of
the string formatter function `fomrat'."
  (let ((cmd (apply #'format fmt arg)))
    (gud-basic-call cmd)))

(defmethod geben-send-command ((this <geben-dbgp-session>) operand &rest params)
  "Send a command to a debugger engine.
CMD is a `<geben-dbgp-cmd> object.
This function automatically inserts a transaction ID which is
required for each dbgp command by the protocol specification."
  (let ((cmd (<geben-dbgp-cmd> operand :operand operand :params params))
	(tid (geben-mktid this)))
    (geben-store-cmd this tid cmd)
    (gud-basic-call (geben-build-send-command cmd tid))
    tid))

;;;
;;; command/response handlers
;;;

;; step_into

(defmethod geben-command-step-into ((this <geben-dbgp-session>))
  "Send \`step_into\' command."
  (geben-send-command this "step_into"))

(defmethod geben-response-step-into ((this <geben-dbgp-session>) cmd msg)
  "A response message handler for a \`step_into\' command."
  nil)

;; step_over

(defmethod geben-command-step-over ((this <geben-dbgp-session>))
  "Send \`step_over\' command."
  (geben-send-command this "step_over"))

(defmethod geben-response-step-over ((this <geben-dbgp-session>) cmd msg)
  "A response message handler for a \`step_over\' command."
  nil)

;; step_out
(defmethod geben-response-step-out ((this <geben-dbgp-session>) cmd msg)
  "A response message handler for a \`step_out\' command."
  nil)

(defmethod geben-command-step-out ((this <geben-dbgp-session>))
  "Send \`step_out\' command."
  (geben-send-command this "step_out"))

;; run

(defmethod geben-command-run ((this <geben-dbgp-session>))
  "Send \`run\' command."
  (geben-send-command this "run"))

(defmethod geben-response-run ((this <geben-dbgp-session>) cmd msg)
  "A response message handler for a \`run\' command."
  nil)

;;; stop

(defmethod geben-command-stop ((this <geben-dbgp-session>))
  "Send \`stop\' command."
  (geben-send-command this "stop"))

(defmethod geben-response-stop ((this <geben-dbgp-session>) cmd msg)
  "A response message handler for a \`stop\' command."
  nil)

;;; breakpoint_set

(defmethod geben-command-breakpoint-set ((this <geben-dbgp-session>)
					 &optional force fileuri path lineno)
  "Send \`breakpoint_set\' command."
  (setq path (or path
		 (buffer-file-name (current-buffer))))
  (when path
    (setq lineno (or lineno
		     (and (get-file-buffer path)
			  (with-current-buffer (get-file-buffer path)
			    (geben-what-line)))))
    (setq fileuri (or fileuri
		      (geben-find-fileuri this path)
		      (concat "file://" (file-truename path))))
    (when (or force
	      (null (geben-find-lineno-breakpoint fileuri lineno)))
      (geben-send-command
       this
       "breakpoint_set"
       (cons "-t" "line")
       (cons "-f" fileuri)
       (cons "-n" lineno)))))

(defmethod geben-response-breakpoint-set ((this <geben-dbgp-session>) cmd msg)
  "A response message handler for a \`breakpoint_set\' command."
  (let ((type (geben-get-param-arg cmd "-t"))
	(id (xml-get-attribute msg 'id)))
    (cond
     ((equal type "line")
      (let* ((fileuri (geben-get-param-arg cmd "-f"))
	     (lineno (geben-get-param-arg cmd "-n"))
	     (path (or (geben-get-local-path this fileuri)
		       (geben-temp-path-for-fileuri fileuri))))
	(geben-add-lineno-breakpoint fileuri path lineno id))))))

;;; breakpoint_remove

(defmethod geben-command-breakpoint-remove ((this <geben-dbgp-session>)
					    &optional fileuri path lineno)
  "Send \`breakpoint_remove\' command."
  (setq path (or path
		 (buffer-file-name (current-buffer))))
  (when path
    (setq lineno (or lineno
		     (and (get-file-buffer path)
			  (with-current-buffer (get-file-buffer path)
			    (geben-what-line)))))
    (setq fileuri (or fileuri
		      (geben-find-fileuri this path)
		      (concat "file://" (file-truename path))))
    (when (and fileuri lineno)
      (let ((bp (geben-find-lineno-breakpoint fileuri lineno)))
	(when bp
	  (geben-send-command
	   this
	   "breakpoint_remove"
	   (cons "-d" (oref bp id))))))))

(defmethod geben-response-breakpoint-remove ((this <geben-dbgp-session>) cmd msg)
  "A response message handler for a \`breakpoint_remove\' command."
  (let* ((bp (car-safe (xml-get-children msg 'breakpoint)))
	 (id (xml-get-attribute bp 'id)))
    (geben-remove-breakpoint id)))

;;; stack_get

(defmethod geben-command-stack-trace ((this <geben-dbgp-session>))
  "Send \`stack_get\' command."
  (geben-send-command this "stack_get"))

(defmethod geben-response-stack-get ((this <geben-dbgp-session>) cmd msg)
  "A response message handler for a \`stack_get\' command."
  (let* ((stack (car-safe (xml-get-children msg 'stack)))
	 (fileuri (xml-get-attribute stack 'filename))
	 (lineno (xml-get-attribute stack'lineno)))
    (geben-ecb-stacktree-update (xml-get-children msg 'stack))
    (when (and fileuri lineno)
      (let ((path (geben-get-local-path this fileuri)))
	(if path
	    (progn
	      (setq gud-last-frame
		    (cons path (string-to-int lineno)))
	      (message "stopped: %s(%s)"
		       (file-name-nondirectory path) lineno))
	  (geben-setup-source-file this fileuri)
	  (geben-command-stack-trace this))))))

(defmethod geben-setup-source-file ((this <geben-dbgp-session>) fileuri)
  "Setup a source file.

if the current session's member variable \`remotep\' is nil and the
file specified by FILEURI is exists, do nothing but just put info into
SOURCE-FILES member variable of the current session.
Otherwise call a dbgp command \`source\' to create copy of the source
code."
  (unless (geben-get-local-path this fileuri)
    (let ((local-path (geben-make-local-path fileuri)))
      (if (or (oref this remotep)
	      (not (file-exists-p local-path)))
	  (geben-command-source this fileuri)
	(let ((source-code (<geben-local-source-code> fileuri
						      :fileuri fileuri
						      :path local-path)))
	  (puthash fileuri source-code (oref this source-files))
	  (geben-visit-file source-code))))))

;;; eval

(defmethod geben-command-eval ((this <geben-dbgp-session>) exp)
  "Send \`eval\' command."
  (geben-send-command
   this
   "eval"
   (format "-- {%s}" (base64-encode-string exp))))

(defmethod geben-response-eval ((this <geben-dbgp-session>) cmd msg)
  "A response message handler for a \`eval\' command."
  (message "result: %S" 
	   (geben-dbgp-decode-value (car-safe (xml-get-children msg 'property)))))

(defun geben-dbgp-decode-value (prop)
  "Decode a VALUE passed by xdebug engine."
  (let ((type (xml-get-attribute prop 'type))
	result)
    (setq result
	  (cond
	   ((string= "array" type)
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
		  value
		(string-to-number value))))))
    (let ((name (xml-get-attribute prop 'name)))
      (if (string< "" name)
	  (cons name result)
	result))))
	   
;;; source

(defmethod geben-command-source ((this <geben-dbgp-session>) fileuri)
  "Send source command.
FILEURI is a uri of the target file of a debuggee site."
  (geben-send-command this "source" (cons "-f" fileuri)))

(defmethod geben-response-source ((this <geben-dbgp-session>) cmd msg)
  "A response message handler for a \`source\' command."
  (let* ((fileuri (geben-get-param-arg cmd "-f"))
	;; (decode-coding-string (base64-decode-string (third msg)) 'undecided)))))
	 (path (geben-store-source-content this fileuri
					   (base64-decode-string (third msg)))))
    (when path
      (let ((source-code (gethash fileuri (oref this source-files))))
	(if source-code
	    (geben-update-file-content source-code path)
	  (setq source-code (<geben-remote-source-code> fileuri
							:fileuri fileuri
							:path path))
	  (puthash fileuri source-code (oref this source-files)))
	(geben-visit-file source-code)))))

(defmethod geben-store-source-content ((this <geben-dbgp-session>) fileuri source)
  (let ((path (geben-temp-path-for-fileuri fileuri)))
    (when path
      (make-directory (file-name-directory path) t)
      (with-current-buffer (find-file-noselect path)
	(widen)
	(erase-buffer)
	(font-lock-mode nil)
	(let ((encoding (detect-coding-string source t)))
	  (unless (eq 'undecided encoding)
	    (set-buffer-file-coding-system encoding))
	  (insert (decode-coding-string source encoding)))
	(with-temp-message ""
	  (save-buffer))
	(kill-buffer (current-buffer)))
      path)))

(defmethod geben-command-feature-get ((this <geben-dbgp-session>) feature)
  "Send \`feature_get\' command."
  (geben-send-command this "feature_get") feature)

(defmethod geben-response-feature-get ((this <geben-dbgp-session>) cmd msg)
  "A response message handler for a \`feature_get\' command."
  nil)

;;;

(defmethod geben-find-fileuri ((this <geben-dbgp-session>) path)
  "Find fileuri for PATH."
  (let (fileuri)
    (maphash (lambda (key source-code)
	       (when (geben-path-equal-p source-code path)
		 ;; todo: how can I stop this iteration?
		 (setq fileuri key)))
	     (oref this source-files))
    fileuri))
	     
(defmethod geben-get-local-path ((this <geben-dbgp-session>) fileuri)
  (let ((src (gethash fileuri (oref this source-files))))
    (when src
      (oref src path))))

(provide 'geben-dbgp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; geben-dbgp.el ends here
