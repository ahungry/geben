;;; geben.el --- DBGp protocol frontend, a script debugger

;; Copyright (C) 2005-2010  reedom <fujinaka.tohru@gmail.com>
;; Copyright (C) 2016  Matthew Carter

;; Filename: geben.el
;; Author: Matthew Carter <m@ahungry.com>
;; Author: Johannes Goslar <jogo@kronberger-spiele.de>
;; Code derived from Original Author: reedom <fujinaka.tohru@gmail.com>
;; Maintainer: Matthew Carter <m@ahungry.com>
;; URL: https://github.com/ahungry/geben
;; Version: 1.1.1
;; Keywords: c, comm, tools
;; Compatibility: Emacs 24+
;; Package-Requires: ((emacs "24.3") (cl-lib "0.5"))

;; This file is not part of GNU Emacs

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; GEBEN is a software package that interfaces Emacs to DBGp protocol
;; with which you can debug running scripts interactively.  At this
;; present time, DBGp protocols are supported in several script
;; languages with help of custom extensions.

;;; Usage

;; 1. Insert autoload hooks into your .Emacs file.
;;    -> (autoload 'geben "geben" "DBGp protocol frontend, a script debugger" t)
;; 2. Start GEBEN.  By default, M-x geben will start it.
;;    GEBEN starts to listening to DBGp protocol session connection.
;; 3. Run debuggee script.
;;    When the connection is established, GEBEN loads the entry script
;;    file in geben-mode.
;; 4. Start debugging.  To see geben-mode key bindings, type ?.

;;; Requirements:

;; [Server side]
;; - PHP with Xdebug 2.0.3
;;    http://xdebug.org/
;; - Perl, Python, Ruby, Tcl with Komodo Debugger Extension
;;    http://aspn.activestate.com/ASPN/Downloads/Komodo/RemoteDebugging

;; [Client side]
;; - Emacs 24 and later

;;; Code:

(eval-when-compile
  (when (or (not (boundp 'emacs-version))
            (string< emacs-version "24"))
    (error (concat "geben.el: This package requires Emacs 24 or later.")))
  (require 'cl))

(eval-and-compile
  (require 'cl-lib)
  (require 'xml)
  (require 'tree-widget)
  (require 'dbgp))

(defvar geben-version "1.1.1")

;;--------------------------------------------------------------
;; customization
;;--------------------------------------------------------------

;; customize group

(defgroup geben nil
  "A PHP Debugging environment."
  :group 'debug)

(defgroup geben-highlighting-faces nil
  "Faces for GEBEN."
  :group 'geben
  :group 'font-lock-highlighting-faces)

;; display window behavior

(defvar geben-dynamic-property-buffer-p nil)

(defcustom geben-display-window-function 'pop-to-buffer
  "*Function to display a debuggee script's content.
Typically `pop-to-buffer' or `switch-to-buffer'."
  :group 'geben
  :type 'function)

(defcustom geben-show-redirect-buffers t
  "Shall stdout/stderr buffers be shown automatically."
  :group 'geben
  :type 'boolean)

(defsubst geben-dbgp-dynamic-property-bufferp (buf)
  (with-current-buffer buf
    (symbol-value 'geben-dynamic-property-buffer-p)))

(defun geben-dbgp-display-window (buf)
  "Display a buffer BUF anywhere in a window, depends on the circumstance."
  (cond
   (geben-full-frame-first-buffer
    (setq geben-full-frame-first-buffer nil)
    (switch-to-buffer buf))
   ((get-buffer-window buf)
    (select-window (get-buffer-window buf))
    (switch-to-buffer buf))
   ((or (eq 1 (count-windows))
        (not (geben-dbgp-dynamic-property-buffer-visiblep)))
    (funcall geben-display-window-function buf))
   (t
    (let ((candidates (make-vector 3 nil))
          (dynamic-p (geben-dbgp-dynamic-property-bufferp buf)))
      (cl-block finder
        (walk-windows (lambda (window)
                        (if (geben-dbgp-dynamic-property-bufferp (window-buffer window))
                            (if dynamic-p
                                (unless (aref candidates 1)
                                  (aset candidates 1 window)))
                          (if (eq (selected-window) window)
                              (aset candidates 2 window)
                            (aset candidates 0 window)
                            (cl-return-from finder))))))
      (select-window (or (aref candidates 0)
                         (aref candidates 1)
                         (aref candidates 2)
                         (selected-window)))
      (switch-to-buffer buf))))
  buf)

;;  (when (buffer-live-p buf)
;;    (or (eq buf (get-buffer geben-context-buffer-name))
;;	(eq buf (get-buffer (geben-dbgp-redirect-buffer-name session :stdout)))
;;	(eq buf (get-buffer (geben-dbgp-redirect-buffer-name session :stderr))))))

(defun geben-dbgp-dynamic-property-buffer-visiblep ()
  "Check whether any window displays any property buffer."
  (cl-block walk-loop
    (walk-windows (lambda (window)
                    (if (geben-dbgp-dynamic-property-bufferp (window-buffer window))
                        (cl-return-from walk-loop t))))))


;;==============================================================
;; utilities
;;==============================================================

(defun geben-rec (x acc)
  "Helper function for recursively flattening a list, where X is the list and ACC is the accumulator."
  (cond ((null x) acc)
        ((atom x) (cons x acc))
        (t (geben-rec (car x) (geben-rec (cdr x) acc)))))

(defsubst geben-flatten (x)
  "Make cons X to a flat list."
  (geben-rec x nil))

(defsubst geben-what-line (&optional pos)
  "Get the number of the line in which POS is located.
If POS is omitted, then the current position is used."
  (save-restriction
    (widen)
    (save-excursion
      (if pos (goto-char pos))
      (beginning-of-line)
      (1+ (count-lines 1 (point))))))

(defmacro geben-plist-push (plist prop value)
  `(let* ((plist ,plist)
          (l (plist-get plist ,prop)))
     (cond
      ((consp l)
       (plist-put plist ,prop
                  (cons ,value (plist-get plist ,prop))))
      ((null l)
       (plist-put plist ,prop (list ,value)))
      (t
       (error "geben-plist-push: cannot add value; type of prop `%s' is not `list' but `%s'."
              ,prop (type-of ,value))))))

(defmacro geben-plist-append (plist prop value)
  `(let* ((plist ,plist)
          (l (plist-get plist ,prop)))
     (cond
      ((consp l)
       (nconc l (list ,value)))
      ((null l)
       (plist-put plist ,prop (list ,value)))
      (t
       (error "geben-plist-add: cannot add value; type of prop `%s' is not `list' but `%s'."
              ,prop (type-of ,value))))))

(defmacro geben-lexical-bind (bindings &rest body)
  (declare (indent 1)
           (debug (sexp &rest form)))
  (macroexpand-all
   (nconc
    (list 'lexical-let (mapcar (lambda (arg)
                                 (list arg arg))
                               bindings))
    body)))

(defun geben-remove-directory-tree (basedir)
  (ignore-errors
    (mapc (lambda (path)
            (cond
             ((or (file-symlink-p path)
                  (file-regular-p path))
              (delete-file path))
             ((file-directory-p path)
              (let ((name (file-name-nondirectory path)))
                (or (equal "." name)
                    (equal ".." name)
                    (geben-remove-directory-tree path))))))
          (directory-files basedir t nil t))
    (delete-directory basedir)))

(defun geben-remote-p (ip)
  "Test whether IP refers a remote system."
  (not (or (equal ip "127.0.0.1")
           (and (fboundp 'network-interface-list)
                (member ip (mapcar (lambda (addr)
                                     (format-network-address (cdr addr) t))
                                   (network-interface-list)))))))

;;--------------------------------------------------------------
;;  cross emacs overlay definitions
;;--------------------------------------------------------------

(eval-and-compile
  (and (featurep 'xemacs)
       (require 'overlay))
  (or (fboundp 'overlay-livep)
      (defalias 'overlay-livep 'overlay-buffer)))

(defun geben-overlay-make-line (lineno &optional buf)
  "Create a whole line overlay on LINENO line.
Optionally, in buffer BUF."
  (with-current-buffer (or buf (current-buffer))
    (save-excursion
      (widen)
      (goto-line lineno)
      (beginning-of-line)
      (make-overlay (point)
                    (save-excursion
                      (forward-line) (point))
                    nil t nil))))


;;==============================================================
;; DBGp related utilities
;;==============================================================

(cl-defmacro geben-dbgp-sequence (cmd &rest callback)
  (declare (indent 1)
           (debug (form &rest form)))
  (list 'progn
        (list 'geben-plist-append cmd
              :callback (car callback))))

(cl-defmacro geben-dbgp-sequence-bind (bindings cmd callback)
  (declare (indent 1)
           (debug (sexp form lambda-expr)))
  (macroexpand-all
   (list 'progn
         (list 'geben-plist-append cmd
               :callback (if bindings
                             (list 'geben-lexical-bind bindings callback)
                           callback)))))

(defun geben-dbgp-decode-string (string data-encoding coding-system)
  "Decode encoded STRING.
Use the DATA-ENCODING appropriate to the CODING-SYSTEM."
  (when string
    (let ((s string))
      (when (consp s)
        (setq s (car s)))
      (when (stringp s)
        (setq s (cond
                 ((equal "base64" data-encoding)
                  (base64-decode-string s))
                 (t s)))
        (if coding-system
            (decode-coding-string s coding-system)
          s)))))


(defcustom geben-temporary-file-directory (expand-file-name "geben" user-emacs-directory)
  "*Base directory path where GEBEN creates temporary files and directories."
  :group 'geben
  :type 'directory)

(defvar geben-storages nil)
(defvar geben-storage-loaded nil)

(defun geben-storage-load ()
  (let ((storage-path (expand-file-name ".storage"
                                        geben-temporary-file-directory)))
    (when (file-exists-p storage-path)
      (ignore-errors
        (with-temp-buffer
          (insert-file-contents storage-path)
          (setq geben-storages (read (buffer-string))))))))

(defun geben-storage-save ()
  (let ((storage-path (expand-file-name ".storage"
                                        geben-temporary-file-directory)))
    (with-temp-buffer
      (pp geben-storages (current-buffer))
      (with-temp-message ""
        (write-region (point-min) (point-max) storage-path)))))


;;==============================================================
;; session
;;==============================================================

;;--------------------------------------------------------------
;; constants
;;--------------------------------------------------------------

(defconst geben-process-buffer-name "*GEBEN<%s> process*"
  "Name for DBGp client process console buffer.")
(defconst geben-backtrace-buffer-name "*GEBEN<%s> backtrace*"
  "Name for backtrace buffer.")
(defconst geben-breakpoint-list-buffer-name "*GEBEN<%s> breakpoint list*"
  "Name for breakpoint list buffer.")
(defconst geben-context-buffer-name "*GEBEN<%s> context*"
  "Name for context buffer.")

(defvar geben-sessions nil)
(defvar geben-current-session nil)

;; geben session start/finish hooks

(defcustom geben-session-enter-hook nil
  "*Hook running at when the geben debugging session is starting.
Each function is invoked with one argument, SESSION"
  :group 'geben
  :type 'hook)

(defcustom geben-session-exit-hook nil
  "*Hook running at when the geben debugging session is finished."
  :group 'geben
  :type 'hook)

(defcustom geben-pause-at-entry-line t
  "*Specify whether debuggee script should be paused at the entry line.
If the value is t, GEBEN will automatically pause the starting program
at the entry line of the script."
  :group 'geben
  :type 'boolean)

(cl-defstruct (geben-session
               (:constructor nil)
               (:constructor geben-session-make))
  "Represent a DBGp protocol connection session."
  storage
  process
  (tid 30000)
  (state :created)
  initmsg
  xdebug-p
  language
  feature
  redirect
  breakpoint
  cmd
  sending-p
  source
  stack
  context
  (cursor (list :overlay nil :position nil))
  tempdir
  )

(defmacro geben-with-current-session (binding &rest body)
  (declare (indent 1)
           (debug (symbolp &rest form)))
  (macroexpand-all
   `(let ((,binding geben-current-session))
      (when ,binding
        ,@body))))

;; initialize

(defsubst geben-session-init (session init-msg)
  "Initialize a SESSION of a process PROC, with the INIT-MSG."
  (geben-session-tempdir-setup session)
  (setf (geben-session-initmsg session) init-msg)
  (setf (geben-session-xdebug-p session)
        (equal "Xdebug" (car (xml-node-children
                              (car (xml-get-children init-msg 'engine))))))
  (setf (geben-session-language session)
        (let ((lang (xml-get-attribute-or-nil init-msg 'language)))
          (and lang
               (intern (concat ":" (downcase lang))))))
  (setf (geben-session-storage session) (or (geben-session-storage-find session)
                                            (geben-session-storage-create session)))
  (run-hook-with-args 'geben-session-enter-hook session))

(defun geben-session-storage-create (session)
  "Create the necessary storage for the SESSION."
  (let* ((initmsg (geben-session-initmsg session))
         (process (geben-session-process session))
         (listener (dbgp-plist-get process :listener))
         (storage (if (dbgp-proxy-p process)
                      (list :proxy t
                            :addr (xml-get-attribute initmsg 'hostname)
                            :idekey (xml-get-attribute initmsg 'idekey))
                    (list :proxy nil
                          :port (cl-second (process-contact listener))))))
    (nconc storage (list :language (geben-session-language session)
                         :fileuri (xml-get-attribute initmsg 'fileuri)))
    (add-to-list 'geben-storages storage)
    storage))

(defun geben-session-storage-find (session)
  (unless geben-storage-loaded
    (geben-storage-load)
    (setq geben-storage-loaded t))
  (let* ((initmsg (geben-session-initmsg session))
         (addr (xml-get-attribute initmsg 'hostname))
         (fileuri (xml-get-attribute initmsg 'fileuri))
         (idekey (xml-get-attribute initmsg 'idekey))
         (process (geben-session-process session))
         (listener (dbgp-plist-get process :listener))
         (proxy-p (dbgp-proxy-p listener))
         (port (cl-second (process-contact listener))))
    (cl-find-if (lambda (storage)
                  (and (eq (not proxy-p)
                           (not (plist-get storage :proxy)))
                       (eq (geben-session-language session)
                           (plist-get storage :language))
                       (equal fileuri (plist-get storage :fileuri))
                       (if proxy-p
                           (and (equal addr (plist-get storage :addr))
                                (equal idekey (plist-get storage :idekey)))
                         (eq port (plist-get storage :port)))))
                geben-storages)))

(defsubst geben-session-release (session)
  "Initialize a SESSION of a process PROC."
  (setf (geben-session-process session) nil)
  (setf (geben-session-cursor session) nil)
  (geben-session-tempdir-remove session)
  (geben-storage-save)
  (run-hook-with-args 'geben-session-exit-hook session))

(defsubst geben-session-active-p (session)
  "Evaluate the active state of SESSION.  If active, will return t, otherwise nil."
  (let ((proc (geben-session-process session)))
    (and (processp proc)
         (eq 'open (process-status proc)))))

;; tid

(defsubst geben-session-next-tid (session)
  "Get transaction id for next command in SESSION."
  (prog1
      (geben-session-tid session)
    (cl-incf (geben-session-tid session))))

;; buffer

(defsubst geben-session-buffer-name (session format-string)
  "Return the buffer name for SESSION, formatted according to FORMAT-STRING."
  (let* ((proc (geben-session-process session))
         (idekey (plist-get (dbgp-proxy-get proc) :idekey)))
    (format format-string
            (concat (if idekey
                        (format "%s:" idekey)
                      "")
                    (format "%s:%s"
                            (dbgp-ip-get proc)
                            (dbgp-port-get (dbgp-listener-get proc)))))))

(defsubst geben-session-buffer (session format-string)
  (get-buffer-create (geben-session-buffer-name session format-string)))

(defsubst geben-session-buffer-get (session format-string)
  (get-buffer (geben-session-buffer-name session format-string)))

(defsubst geben-session-buffer-live-p (session format-string)
  (buffer-live-p (get-buffer (geben-session-buffer-name session format-string))))

(defsubst geben-session-buffer-visible-p (session format-string)
  (let ((buf (get-buffer (geben-session-buffer-name session format-string))))
    (and buf
         (buffer-live-p buf)
         (get-buffer-window buf))))

(defun geben-kill-buffers (&rest args)
  "Kills all buffers whose name start with *GEBEN.
 ARGS is so it can be used for `geben-session-exit-hook'.

 TODO: Standard buffer names so we don't have to do a string match."
  (mapc (lambda(buffer)
          (if (string-prefix-p "*GEBEN" (buffer-name buffer))
              (kill-buffer buffer)))
        (buffer-list)))

;; temporary directory

(defun geben-session-tempdir-setup (session)
  "Setup temporary directory for SESSION."
  (let* ((proc (geben-session-process session))
         (gebendir (file-truename geben-temporary-file-directory))
         (leafdir (format "%d" (cl-second (process-contact proc))))
         (tempdir (expand-file-name leafdir gebendir)))
    (unless (file-directory-p gebendir)
      (make-directory gebendir t)
      (set-file-modes gebendir #o1777))
    (setf (geben-session-tempdir session) tempdir)))

(defun geben-session-tempdir-remove (session)
  "Remove temporary directory for SESSION."
  (let ((tempdir (geben-session-tempdir session)))
    (when (file-directory-p tempdir)
      (geben-remove-directory-tree tempdir))))

;; misc

(defsubst geben-session-ip-get (session)
  "Get ip address of the host server in SESSION."
  (let* ((proc (geben-session-process session))
         (listener (dbgp-listener-get proc)))
    (format-network-address (dbgp-ip-get proc) t)))

(defun geben-session-remote-p (session)
  "Get ip address of the host server IN SESSION."
  (geben-remote-p (geben-session-ip-get session)))


;;==============================================================
;; cmd hash
;;==============================================================

(defmacro geben-cmd-param-for (key)
  `(plist-get '(:depth "-d"
                       :context-id "-c"
                       :max-data-size "-m"
                       :type "-t"
                       :page "-p"
                       :key "k"
                       :address "-a"
                       :name "-n"
                       :fileuri "-f"
                       :lineno "-n"
                       :class "-a"
                       :function "-m"
                       :state "-s"
                       :exception "-x"
                       :hit-value "-h"
                       :hit-condition "-o"
                       :run-once "-r"
                       :expression "--")
              ,key))

(defsubst geben-cmd-param-get (cmd flag)
  "For CMD, get FLAG's parameter used.
For a DBGp command \`stack_get -i 1 -d 2\',
`(geben-cmd-param-get cmd \"-d\")\' gets \"2\"."
  (cdr-safe (assoc flag (plist-get cmd :param))))

(defun geben-cmd-expand (cmd)
  "Build a send command CMD string for DBGp protocol."
  (mapconcat #'(lambda (x)
                 (cond ((stringp x) x)
                       ((integerp x) (int-to-string x))
                       ((atom (format "%S" x)))
                       ((null x) "")
                       (t x)))
             (geben-flatten (list (plist-get cmd :operand)
                                  "-i"
                                  (plist-get cmd :tid)
                                  (plist-get cmd :param)))
             " "))

(defsubst geben-session-cmd-make (session operand params)
  "Create a new command object for SESSION.
Assign OPERAND to :operand, and PARAMS to :param in the plist."
  (list :session session
        :tid (geben-session-next-tid session)
        :operand operand
        :param params))

(defsubst geben-session-cmd-append (session cmd)
  (let ((cmds (geben-session-cmd session)))
    (if cmds
        (nconc cmds (list cmd))
      (setf (geben-session-cmd session) (list cmd)))))

(defun geben-session-cmd-remove (session tid)
  "Get a command object from the command hash table for SESSION specified by TID."
  (let ((cmds (geben-session-cmd session)))
    (if (eq tid (plist-get (car cmds) :tid))
        (prog1
            (car cmds)
          (setf (geben-session-cmd session) (cdr cmds)))
      (let (match-cmd)
        (setf (geben-session-cmd session)
              (cl-remove-if (lambda (cmd)
                              (and (eq tid (plist-get cmd :tid))
                                   (setq match-cmd cmd)))
                            cmds))
        match-cmd))))


;;==============================================================
;; DBGp protocol handler
;;==============================================================

(defsubst geben-dbgp-tid-read (msg)
  "Get a transaction id of MSG."
  (let ((tid (xml-get-attribute-or-nil msg 'transaction_id)))
    (and tid
         (string-to-number tid))))

(defun geben-dbgp-entry (session msg)
  "Within SESSION, analyze MSG and dispatch to a specific handler."
  ;; remain session status ('connect, 'init, 'break, 'stopping, 'stopped)
  (let ((handler (intern-soft (concat "geben-dbgp-handle-"
                                      (symbol-name (xml-node-name msg)))))
        (status (xml-get-attribute-or-nil msg 'status)))
    (and status
         (setf (geben-session-state session) (intern (concat ":" status))))
    (and (functionp handler)
         (funcall handler session msg))))

(defvar geben-dbgp-init-hook nil)

(defun geben-dbgp-handle-init (session msg)
  "Within SESSION, handle a init message MSG."
  (geben-session-init session msg)
  (run-hook-with-args 'geben-dbgp-init-hook session))

(defun geben-dbgp-handle-response (session msg)
  "Within SESSION, handle a response message MSG."
  (let* ((tid (geben-dbgp-tid-read msg))
         (cmd (geben-session-cmd-remove session tid))
         (err (dbgp-xml-get-error-node msg)))
    (geben-dbgp-handle-status session msg)
    (geben-dbgp-process-command-queue session)
    (cond
     (err
      (message "Command error: %s"
               (dbgp-xml-get-error-message msg)))
     (cmd
      (let* ((operand (replace-regexp-in-string
                       "_" "-" (xml-get-attribute msg 'command)))
             (func-name (concat "geben-dbgp-response-" operand))
             (func (intern-soft func-name)))
        (and (functionp func)
             (funcall func session cmd msg)))))
    (mapc (lambda (callback)
            (funcall callback session cmd msg err))
          (plist-get cmd :callback))))

(defun geben-dbgp-handle-status (session msg)
  "Within SESSION, handle status code in a response message MSG."
  (let ((status (xml-get-attribute msg 'status)))
    (cond
     ((equal status "stopping")
      (accept-process-output)
      (and (geben-session-active-p session)
           (geben-dbgp-command-stop session))))))

;;; command sending

(defun geben-dbgp-send-string (session string)
  (and (string< "" string)
       (geben-session-active-p session)
       (dbgp-session-send-string (geben-session-process session) string t)))

(defun geben-send-raw-command (session fmt &rest arg)
  "Send a command string to a debugger engine for SESSION.
The command string will be built up with FMT and ARG with a help of
the string formatter function `format'."
  (let ((cmd (apply #'format fmt arg)))
    (geben-dbgp-send-string session cmd)))

(defun geben-dbgp-send-command (session operand &rest params)
  "Send a command to a debugger engine for SESSION.
OPERAND and PARAMS will be passed along to 'geben-session-cmd-make.
Return a cmd list."
  (if (geben-session-active-p session)
      (let ((cmd (geben-session-cmd-make session operand params)))
        (geben-session-cmd-append session cmd)
        (unless (geben-session-sending-p session)
          (setf (geben-session-sending-p session) t)
          (geben-dbgp-process-command-queue session))
        cmd)))

(defun geben-dbgp-process-command-queue (session)
  (let ((cmd (car (geben-session-cmd session))))
    (if cmd
        (geben-dbgp-send-string session (geben-cmd-expand cmd))
      (setf (geben-session-sending-p session) nil))))

(defvar geben-dbgp-continuous-command-hook nil)

;;--------------------------------------------------------------
;; continuous commands
;;--------------------------------------------------------------

;; step_into

(defun geben-dbgp-command-step-into (session)
  "Send \`step_into\' command to the SESSION."
  (geben-dbgp-send-command session "step_into"))

(defun geben-dbgp-response-step-into (session cmd msg)
  "For SESSION, send a CMD response MSG handler for \`step_into\'."
  (run-hook-with-args 'geben-dbgp-continuous-command-hook session))

;; step_over

(defun geben-dbgp-command-step-over (session)
  "Send \`step_over\' command to the SESSION."
  (geben-dbgp-send-command session "step_over"))

(defun geben-dbgp-response-step-over (session cmd msg)
  "For SESSION, send a CMD response MSG handler for \`step_over\'."
  (run-hook-with-args 'geben-dbgp-continuous-command-hook session))

;; step_out

(defun geben-dbgp-command-step-out (session)
  "Send \`step_out\' command to the SESSION."
  (geben-dbgp-send-command session "step_out"))

(defun geben-dbgp-response-step-out (session cmd msg)
  "A response message handler for \`step_out\' command."
  (run-hook-with-args 'geben-dbgp-continuous-command-hook session))

;; run

(defun geben-dbgp-command-run (session)
  "Send \`run\' command."
  (geben-dbgp-send-command session "run"))

(defun geben-dbgp-response-run (session cmd msg)
  "A response message handler for \`run\' command."
  (run-hook-with-args 'geben-dbgp-continuous-command-hook session))

;;; stop

(defun geben-dbgp-command-stop (session)
  "Send \`stop\' command."
  (geben-dbgp-send-command session "stop"))

;;; eval

(defun geben-dbgp-command-eval (session exp)
  "Send \`eval\' command."
  (geben-dbgp-send-command
   session
   "eval"
   (format "-- {%s}" (base64-encode-string exp))))

(defun geben-dbgp-response-eval (session cmd msg)
  "A response message handler for \`eval\' command."
  (message "result: %S"
           (geben-dbgp-decode-value (car-safe (xml-get-children msg 'property)))))

(defun geben-dbgp-decode-value (prop)
  "Decode a VALUE passed by debugger engine."
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
              (cl-assert (stringp value))
              (when (string= "base64" (xml-get-attribute prop 'encoding))
                (setq value (base64-decode-string value)))
              (if (string= "string" type)
                  (decode-coding-string value 'utf-8)
                (string-to-number value))))))
    (let ((name (xml-get-attribute-or-nil prop 'name)))
      (if name
          (cons name result)
        result))))

(eval-when-compile
  (require 'tramp))

;;==============================================================
;; source
;;==============================================================

;; file hooks

(defcustom geben-source-visit-hook nil
  "*Hook running at when GEBEN visits a debuggee script file.
Each function is invoked with one argument, BUFFER."
  :group 'geben
  :type 'hook)

(defcustom geben-close-mirror-file-after-finish t
  "*Specify whether GEBEN should close fetched files from remote site after debugging.
Since the remote files is stored temporary that you can confuse
they were editable if they were left after a debugging session.
If the value is non-nil, GEBEN closes temporary files when
debugging is finished.
If the value is nil, the files left in buffers."
  :group 'geben
  :type 'boolean)

(defun geben-source-find-file-handler ()
  (let* ((local-path (buffer-file-name))
         (session (and local-path (geben-source-find-session local-path))))
    (if session
        (run-hook-with-args 'geben-source-visit-hook session (current-buffer)))))

(add-hook 'find-file-hook #'geben-source-find-file-handler)

;;--------------------------------------------------------------
;; source hash
;;--------------------------------------------------------------

(defcustom geben-source-coding-system 'utf-8
  "Coding system for source code retrieving remotely via the debugger engine."
  :group 'geben
  :type 'coding-system)

(defmacro geben-source-make (fileuri local-path)
  "Create a new source object.
A source object forms a property list with three properties
:fileuri, :remotep and :local-path."
  `(list :fileuri ,fileuri :local-path ,local-path))

(defvar geben-source-release-hook nil)

(defun geben-source-release (source)
  "Release a SOURCE object."
  (let ((buf (find-buffer-visiting (or (plist-get source :local-path) ""))))
    (when buf
      (with-current-buffer buf
        (when (bound-and-true-p geben-mode)
          (run-hooks 'geben-source-release-hook))
        (when geben-close-mirror-file-after-finish
          (set-buffer-modified-p nil)
          (kill-buffer buf))))))

(defsubst geben-source-fileuri-regularize (fileuri)
  ;; for bug of Xdebug 2.0.3 and below:
  (replace-regexp-in-string "%28[0-9]+%29%20:%20runtime-created%20function$" ""
                            fileuri))

(defun geben-source-fileuri (session local-path)
  "Guess a file uri string which counters to LOCAL-PATH."
  (let* ((tempdir (geben-session-tempdir session))
         (templen (length tempdir))
         (tramp-spec (plist-get (geben-session-storage session) :tramp))
         (tramp-spec-len (and tramp-spec (length tramp-spec))))
    (concat "file://"
            (cond
             ((and (< templen (length local-path))
                   (string= tempdir (substring local-path 0 templen)))
              (substring local-path
                         (- templen
                            (if (string< "" (file-name-nondirectory tempdir)) 0 1))))
             ((and tramp-spec
                   (< tramp-spec-len (length local-path))
                   (string= tramp-spec (substring local-path 0 tramp-spec-len)))
              (substring local-path tramp-spec-len))
             (t
              local-path)))))

(defun geben-source-local-path (session fileuri)
  "Generate path string from FILEURI to store temporarily."
  (let ((local-path (geben-source-local-path-in-server session fileuri)))
    (when local-path
      (expand-file-name (substring local-path (if (string-match "^[A-Z]:" local-path) 3 1))
                        (geben-session-tempdir session)))))

(defun geben-source-local-path-in-server (session fileuri &optional disable-completion)
  "Make a path string correspond to FILEURI."
  (when (string-match "^\\(file\\|https?\\):/+" fileuri)
    (let ((path (substring fileuri (1- (match-end 0)))))
      (require 'url-util)
      (setq path (url-unhex-string path))
      (when (string-match "^/[A-Z]:" path) ;; for HTTP server on Windows
        (setq path (substring path 1)))
      (if (and (not disable-completion)
               (string= "" (file-name-nondirectory path)))
          (expand-file-name (geben-source-default-file-name session)
                            path)
        path))))

(defun geben-source-default-file-name (session)
  (cl-case (geben-session-language session)
    (:php "index.php")
    (:python "index.py")
    (:perl "index.pl")
    (:ruby "index.rb")
    (t "index.html")))

(defun geben-source-find-session (temp-path)
  "Find a session which may have a file at TEMP-PATH in its temporary directory tree."
  (cl-find-if (lambda (session)
                (let ((tempdir (geben-session-tempdir session)))
                  (ignore-errors
                    (string= tempdir (substring temp-path 0 (length tempdir))))))
              geben-sessions))

(defun geben-source-visit (local-path)
  "Visit to a local source code file."
  (let ((buf (or (find-buffer-visiting local-path)
                 (if (file-exists-p local-path)
                     (let* ((session (geben-source-find-session local-path))
                            (storage (and session
                                          (geben-session-storage session)))
                            (coding-system (or (plist-get storage :source-coding-system)
                                               geben-source-coding-system)))
                       (if coding-system
                           (let ((coding-system-for-read coding-system)
                                 (coding-system-for-write coding-system))
                             (find-file-noselect local-path))
                         (find-file-noselect local-path)))))))
    (when buf
      (geben-dbgp-display-window buf)
      buf)))

;; session storage

(defun geben-session-source-storage-add (session fileuri)
  (let* ((storage (geben-session-storage session))
         (list (plist-get storage :source)))
    (if (and (string-match "^file:/" fileuri)
             (not (cl-find list fileuri :test #'equal)))
        (if list
            (nconc list (list fileuri))
          (plist-put storage :source (list fileuri))))))

;; session

(defun geben-session-source-init (session)
  "Initialize a source hash table of the SESSION."
  (setf (geben-session-source session) (make-hash-table :test 'equal)))

(add-hook 'geben-session-enter-hook #'geben-session-source-init)

(defun geben-session-source-add (session fileuri local-path content)
  "Add a source object to SESSION."
  (let ((tempdir (geben-session-tempdir session)))
    (unless (file-directory-p tempdir)
      (make-directory tempdir t)
      (set-file-modes tempdir #o0700)))
  (geben-session-source-write-file session local-path content)
  (puthash fileuri (geben-source-make fileuri local-path) (geben-session-source session))
  (geben-session-source-storage-add session fileuri))

(defun geben-session-source-release (session)
  "Release source objects."
  (maphash (lambda (fileuri source)
             (geben-source-release source))
           (geben-session-source session)))

(add-hook 'geben-session-exit-hook #'geben-session-source-release)
(add-hook 'geben-session-exit-hook #'geben-kill-buffers)

(defsubst geben-session-source-get (session fileuri)
  (gethash fileuri (geben-session-source session)))

(defsubst geben-session-source-append (session fileuri local-path)
  (puthash fileuri (list :fileuri fileuri :local-path local-path)
           (geben-session-source session)))

(defsubst geben-session-source-local-path (session fileuri)
  "Find a known local-path that counters to FILEURI."
  (plist-get (gethash fileuri (geben-session-source session))
             :local-path))

(defsubst geben-session-source-fileuri (session local-path)
  "Find a known fileuri that counters to LOCAL-PATH."
  (cl-block geben-session-souce-fileuri
    (maphash (lambda (fileuri path)
               (and (equal local-path (plist-get path :local-path))
                    (cl-return-from geben-session-souce-fileuri fileuri)))
             (geben-session-source session))))

(defsubst geben-session-source-content-coding-system (session content)
  "Guess a coding-system for the CONTENT."
  (or (plist-get (geben-session-storage session) :source-coding-system)
      geben-source-coding-system
      (detect-coding-string content t)))

(defun geben-session-source-write-file (session path content)
  "Write CONTENT to file."
  (make-directory (file-name-directory path) t)
  (ignore-errors
    (with-current-buffer (or (find-buffer-visiting path)
                             (create-file-buffer path))
      (let ((inhibit-read-only t)
            (coding-system (geben-session-source-content-coding-system session content)))
        (buffer-disable-undo)
        (widen)
        (erase-buffer)
        (font-lock-mode 0)
        (unless (eq 'undecided coding-system)
          (set-buffer-file-coding-system coding-system))
        (insert (decode-coding-string content coding-system)))
      (with-temp-message ""
        (write-file path)
        (kill-buffer (current-buffer))))
    t))

;;; dbgp

(defun geben-dbgp-command-source (session fileuri)
  "Send source command.
FILEURI is a uri of the target file of a debuggee site."
  (geben-dbgp-send-command session "source" (cons "-f"
                                                  (geben-source-fileuri-regularize fileuri))))

(defun geben-dbgp-response-source (session cmd msg)
  "A response message handler for \`source\' command."
  (let* ((fileuri (geben-cmd-param-get cmd "-f"))
         (local-path (geben-source-local-path session fileuri)))
    (when local-path
      (geben-session-source-add session fileuri local-path (base64-decode-string (cl-third msg)))
      (geben-source-visit local-path))))

(defun geben-dbgp-source-fetch (session fileuri)
  "Fetch the content of FILEURI."
  ;;(let ((fileuri (geben-dbgp-regularize-fileuri fileuri)))
  (unless (geben-session-source-local-path session fileuri)
    ;; haven't fetched remote source yet; fetch it.
    (geben-dbgp-command-source session fileuri)))

(defcustom geben-visit-remote-file nil
  ""
  :group 'geben
  :type 'function)

(defcustom geben-get-tramp-spec-for nil
  "Function to retrieve TRAMP spec for a file path of a remove server.
This function is called when visiting a remote server file, with
a parameter `remote-path'. (e.g. \"/192.168.1.32:/var/www/index.php\")
If `remote-path' is unknown to the function, it should return nil.
Or return specific TRAMP spec. (e.g. \"/user@example.com:\""
  :group 'geben
  :type 'function)

(defun geben-session-source-visit-original-file (session fileuri &optional disable-completion)
  (let ((target-path
         (geben-path-mappings-from-debug (geben-session-source-read-file-name session fileuri disable-completion))))
    (and target-path
         (prog1
             (find-file target-path)
           (message "visited: %s" target-path)))))

(defun geben-session-source-read-file-name (session fileuri &optional disable-completion)
  (if (geben-session-remote-p session)
      (geben-session-source-read-file-name-remote session fileuri disable-completion)
    (geben-session-source-read-file-name-local session fileuri disable-completion)))

(defun geben-session-source-read-file-name-local (session fileuri &optional disable-completion)
  (let ((local-path (geben-source-local-path-in-server session fileuri disable-completion)))
    ;; local file
    (unless (file-regular-p local-path)
      (while (not (file-regular-p (setq local-path
                                        (read-file-name "Find local file: "
                                                        local-path local-path t ""))))
        (beep)))
    (expand-file-name local-path)))

(defun geben-session-source-read-file-name-remote (session fileuri &optional disable-completion)
  (condition-case nil
      (if (fboundp 'geben-visit-remote-file)
          (funcall geben-visit-remote-file session fileuri)
        (let* ((ip (geben-session-ip-get session))
               (local-path (geben-source-local-path-in-server session fileuri disable-completion))
               (storage (geben-session-storage session))
               (path-prefix (or (plist-get storage :tramp)
                                (and (fboundp 'geben-get-tramp-spec-for)
                                     (funcall 'geben-get-tramp-spec-for
                                              (format "/%s:%s" ip local-path)))))
               (find-file-default (if path-prefix
                                      (concat path-prefix local-path)
                                    (format "/%s:%s" ip local-path))))
          (while (not (tramp-handle-file-regular-p
                       (setq find-file-default (read-file-name "Find remote file: "
                                                               (file-name-directory find-file-default)
                                                               find-file-default t
                                                               (file-name-nondirectory find-file-default)))))
            (beep))
          (require 'tramp)
          (when (tramp-tramp-file-p find-file-default)
            (plist-put storage :tramp (replace-regexp-in-string ":[^:]+$" ":" find-file-default)))
          find-file-default))
    (quit (beep))))


;;==============================================================
;; cursor
;;==============================================================

(defface geben-cursor-arrow-face
  '((((class color))
     :inherit 'default
     :foreground "cyan"))
  "Face to displaying arrow indicator."
  :group 'geben-highlighting-faces)

(defun geben-session-cursor-update (session fileuri lineno)
  (let ((lineno (cond
                 ((numberp lineno)
                  lineno)
                 ((stringp lineno)
                  (string-to-number lineno))))
        (fileuri (geben-source-fileuri-regularize fileuri)))
    (and lineno
         (floatp lineno)
         (setq lineno 1))		; restrict to integer
    (plist-put (geben-session-cursor session) :position (cons fileuri lineno)))
  (geben-session-cursor-indicate session))

(defun geben-session-cursor-indicate (session)
  "Display indication marker at the current breaking point.
if DISPLAY-BUFFERP is non-nil, the buffer contains the breaking point
will be displayed in a window."
  (let* ((cursor (geben-session-cursor session))
         (position (plist-get cursor :position))
         (fileuri (car position))
         (lineno (cdr position))
         (local-path (geben-session-source-local-path session fileuri)))
    (if local-path
        (geben-session-cursor-overlay-update session)
      (geben-dbgp-sequence
          (geben-dbgp-command-source session fileuri)
        (lambda (session cmd msg err)
          (unless err
            (geben-session-cursor-overlay-update session)))))))

(defun geben-session-cursor-overlay-update (session)
  (let* ((cursor (geben-session-cursor session))
         (overlay (plist-get cursor :overlay))
         (position (plist-get cursor :position))
         (fileuri (car position))
         (lineno (cdr position))
         (local-path (and fileuri
                          (geben-session-source-local-path session fileuri))))
    (if (null position)
        (when (overlayp overlay)
          (delete-overlay overlay)
          (plist-put cursor :overlay nil))
      (let ((buf (geben-source-visit local-path))
            pos)
        (when buf
          (with-current-buffer buf
            (ignore-errors
              (save-restriction
                (widen)
                (goto-line lineno)
                (setq pos (point))
                (if (overlayp overlay)
                    (move-overlay overlay pos pos buf)
                  (plist-put cursor :overlay
                             (setq overlay (make-overlay pos pos buf)))
                  (overlay-put overlay
                               'before-string
                               (propertize "x"
                                           'display
                                           (list
                                            '(margin left-margin)
                                            (propertize "=>"
                                                        'face 'geben-cursor-arrow-face))))))
              (set-window-point (get-buffer-window buf) pos))))))))

(defun geben-session-cursor-file-visit-handler (session buf)
  (let ((cursor (geben-session-cursor session))
        (fileuri (geben-session-source-fileuri session (buffer-file-name buf))))
    (and fileuri
         (equal fileuri (car (plist-get cursor :position)))
         (geben-session-cursor-overlay-update session))))

(add-hook 'geben-source-visit-hook #'geben-session-cursor-file-visit-handler)


;;==============================================================
;; breakpoints
;;==============================================================

(cl-defstruct (geben-breakpoint
               (:constructor nil)
               (:constructor geben-breakpoint-make))
  "Breakpoint setting.

types:
  Breakpoint types supported by the current debugger engine.

list:
  Break point list."
  (types '(:line :call :return :exception :conditional))
  list)

(defface geben-breakpoint-face
  '((((class color))
     :foreground "white"
     :background "red1")
    (t :inverse-video t))
  "Face used to highlight various names.
This includes element and attribute names, processing
instruction targets and the CDATA keyword in a CDATA section.
This is not used directly, but only via inheritance by other faces."
  :group 'geben-highlighting-faces)

(defcustom geben-show-breakpoints-debugging-only t
  "*Specify breakpoint markers visibility.
If the value is nil, GEBEN will always display breakpoint markers.
If non-nil, displays the markers while debugging but hides after
debugging is finished."
  :group 'geben
  :type 'boolean)

;;--------------------------------------------------------------
;; breakpoint object
;;--------------------------------------------------------------

;; breakpoint object manipulators

(defun geben-bp-make (session type &rest params)
  "Create a new line breakpoint object."
  (cl-assert (geben-session-p session))
  (let ((bp (append (list :type type) params)))
    ;; force :lineno and :hit-value value to be integer.
    (mapc (lambda (prop)
            (when (stringp (plist-get bp prop))
              (plist-put bp prop (string-to-number (plist-get bp prop)))))
          '(:lineno :hit-value))
    ;; setup overlay
    (when (and (plist-get params :fileuri)
               (plist-get params :lineno)
               (not (plist-get params :overlay)))
      (geben-bp-overlay-setup bp))
    ;; Xdebug issue; generate :class and :method name from :function
    (let ((name (plist-get params :function)))
      (and name
           (geben-session-xdebug-p session)
           (string-match "[:->]" name)
           (plist-put bp :class (replace-regexp-in-string "^\\([^:-]+\\).*" "\\1" name))
           (plist-put bp :method (replace-regexp-in-string "^.*[:>]+" "" name))))
    ;; make sure bp has :state.
    (unless (plist-get params :state)
      (plist-put bp :state "enabled"))
    bp))

(defsubst geben-bp-finalize (bp)
  "Finalize a breakpoint object."
  (let ((overlay (plist-get bp :overlay)))
    (when (overlayp overlay)
      (delete-overlay overlay)))
  bp)

(defsubst geben-bp= (lhs rhs)
  "Return t if two breakpoint object point same thing."
  (and (eq (plist-get lhs :type)
           (plist-get rhs :type))
       (eq (plist-get lhs :lineno)
           (plist-get rhs :lineno))
       (equal (plist-get lhs :fileuri)
              (plist-get rhs :fileuri))
       (equal (plist-get lhs :function)
              (plist-get rhs :function))
       (equal (plist-get lhs :exception)
              (plist-get rhs :exception))
       (equal (plist-get lhs :expression)
              (plist-get rhs :expression))))

;; session storage

(defun geben-session-breakpoint-storage-add (session bp)
  (let* ((storage (geben-session-storage session))
         (list (plist-get storage :bp)))
    (unless (cl-find bp list :test #'geben-bp=)
      (let ((bp-copy (copy-sequence bp)))
        (plist-put bp-copy :overlay nil)
        (if list
            (nconc list (list bp-copy))
          (plist-put storage :bp (list bp-copy)))))))

(defun geben-session-breakpoint-storage-remove (session bp)
  (let* ((storage (geben-session-storage session))
         (list (plist-get storage :bp)))
    (when (cl-find bp list :test #'geben-bp=)
      (plist-put storage :bp (cl-delete bp list :test #'geben-bp=)))))

(defun geben-session-breakpoint-storage-restore (session)
  (let ((storage (geben-session-storage session))
        (breakpoint (geben-session-breakpoint session)))
    (setf (geben-breakpoint-list breakpoint)
          (plist-get storage :bp))))

;; session

(defun geben-session-breakpoint-add (session bp)
  "Add a breakpoint BP to session's breakpoint list."
  (unless (geben-session-breakpoint-find session bp)
    (let* ((breakpoint (geben-session-breakpoint session))
           (list (geben-breakpoint-list breakpoint)))
      (if list
          (nconc list (list bp))
        (setf (geben-breakpoint-list breakpoint) (list bp))))
    (geben-session-breakpoint-storage-add session bp)))

(defun geben-session-breakpoint-remove (session id-or-obj)
  "Remove breakpoints having specific breakpoint id or same meaning objects."
  (setf (geben-breakpoint-list (geben-session-breakpoint session))
        (cl-remove-if (if (stringp id-or-obj)
                          (lambda (bp)
                            (when (string= (plist-get bp :id) id-or-obj)
                              (geben-session-breakpoint-storage-remove session bp)
                              (geben-bp-finalize bp)))
                        (lambda (bp)
                          (when (geben-bp= id-or-obj bp)
                            (geben-session-breakpoint-storage-remove session bp)
                            (geben-bp-finalize bp))))
                      (geben-breakpoint-list (geben-session-breakpoint session)))))

(defun geben-session-breakpoint-find (session id-or-obj)
  "Find a breakpoint.
id-or-obj should be either a breakpoint id or a breakpoint object."
  (cl-find-if
   (if (stringp id-or-obj)
       (lambda (bp)
         (string= (plist-get bp :id) id-or-obj))
     (lambda (bp)
       (geben-bp= id-or-obj bp)))
   (geben-breakpoint-list (geben-session-breakpoint session))))

;; dbgp

(defun geben-dbgp-breakpoint-restore (session)
  "Restore breakpoints against new DBGp session."
  (let ((breakpoints (geben-breakpoint-list (geben-session-breakpoint session)))
        overlay)
    (setf (geben-breakpoint-list (geben-session-breakpoint session)) nil)
    (dolist (bp breakpoints)
      ;; User may edit code since previous debugging session
      ;; so that lineno breakpoints set before may moved.
      ;; The followings try to adjust breakpoint line to
      ;; nearly what user expect.
      (if (and (setq overlay (plist-get bp :overlay))
               (overlayp overlay)
               (overlay-livep overlay)
               (eq (overlay-buffer overlay)
                   (find-buffer-visiting (or (plist-get bp :local-path)
                                             ""))))
          (with-current-buffer (overlay-buffer overlay)
            (save-excursion
              (plist-put bp :lineno (progn
                                      (goto-char (overlay-start overlay))
                                      (geben-what-line))))))
      (geben-dbgp-sequence-bind (bp)
        (geben-dbgp-command-breakpoint-set session bp)
        (lambda (session cmd msg err)
          (geben-bp-finalize bp))))))

(defun geben-breakpoint-remove (session bp-or-list)
  "Remove specified breakpoints."
  (dolist (bp (if (geben-breakpoint-p bp-or-list)
                  (list bp-or-list)
                bp-or-list))
    (let ((bid (plist-get bp :id)))
      (if (and (geben-session-active-p session)
               bid)
          (geben-dbgp-sequence-bind (bid)
            (geben-dbgp-send-command session "breakpoint_remove" (cons "-d" bid))
            (lambda (session cmd msg err)
              ;; remove a stray breakpoint from hash table.
              (when err
                (geben-session-breakpoint-remove session bid))))
        (setf (geben-breakpoint-list (geben-session-breakpoint session))
              (cl-delete-if (lambda (bp1)
                              (geben-bp= bp bp1))
                            (geben-breakpoint-list (geben-session-breakpoint session))))))))

(defun geben-breakpoint-clear (session)
  "Clear all breakpoints."
  (geben-breakpoint-remove session
                           (geben-breakpoint-list (geben-session-breakpoint session))))

(defun geben-breakpoint-find-at-pos (session buf pos)
  (with-current-buffer buf
    (cl-remove-if 'null
                  (mapcar (lambda (overlay)
                            (let ((bp (overlay-get overlay 'bp)))
                              (and (eq :line (plist-get bp :type))
                                   bp)))
                          (overlays-at pos)))))

;; breakpoint list

(defface geben-breakpoint-fileuri
  '((t (:inherit geben-backtrace-fileuri)))
  "Face used to highlight fileuri in breakpoint list buffer."
  :group 'geben-highlighting-faces)

(defface geben-breakpoint-lineno
  '((t (:inherit geben-backtrace-lineno)))
  "Face for displaying line numbers in breakpoint list buffer."
  :group 'geben-highlighting-faces)

(defface geben-breakpoint-function
  '((t (:inherit font-lock-function-name-face)))
  "Face for displaying line numbers in breakpoint list buffer."
  :group 'geben-highlighting-faces)

(defun geben-breakpoint-sort-pred (a b)
  (if (and (stringp (plist-get a :id))
           (equal (plist-get a :id)
                  (plist-get b :id)))
      nil
    (let ((type-rank '(:line 1
                             :call 2
                             :return 3
                             :exception 4
                             :conditional 5
                             :watch 6))
          ax bx cmp)
      (setq cmp (- (plist-get type-rank (plist-get a :type))
                   (plist-get type-rank (plist-get b :type))))
      (if (not (zerop cmp))
          (< cmp 0)
        (cl-case (plist-get a :type)
          (:line
           (setq ax (plist-get a :fileuri))
           (setq bx (plist-get b :fileuri))
           (or (string< ax bx)
               (and (string= ax bx)
                    (< (plist-get a :lineno)
                       (plist-get b :lineno)))))
          (:call
           (string< (plist-get a :function)
                    (plist-get b :function)))
          (:return
           (string< (plist-get a :function)
                    (plist-get b :function)))
          (:exception
           (string< (plist-get a :exception)
                    (plist-get b :exception)))
          (:conditional
           (or (string< (plist-get a :fileuri)
                        (plist-get b :fileuri))
               (progn
                 (setq ax (plist-get a :lineno)
                       bx (plist-get b :lineno))
                 (if (null ax)
                     (not (null ax))
                   (if (null ax)
                       nil
                     (< ax bx))))
               (string< (plist-get a :expression)
                        (plist-get b :expression))))
          (:watch
           (string< (plist-get a :expression)
                    (plist-get b :expression))))))))

;;--------------------------------------------------------------
;; breakpoint list mode
;;--------------------------------------------------------------

(defcustom geben-breakpoint-list-mode-hook nil
  "*Hook running at when GEBEN's breakpoint list buffer is initialized."
  :group 'geben
  :type 'hook)

(defvar geben-breakpoint-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-2] 'geben-breakpoint-list-mode-mouse-goto)
    (define-key map "\C-m" 'geben-breakpoint-list-mode-goto)
    (define-key map "d" 'geben-breakpoint-list-mark-delete)
    (define-key map "u" 'geben-breakpoint-list-unmark)
    (define-key map "x" 'geben-breakpoint-list-execute)
    (define-key map "q" 'geben-quit-window)
    (define-key map "r" 'geben-breakpoint-list-refresh)
    (define-key map "p" 'previous-line)
    (define-key map "n" 'next-line)
    (define-key map "?" 'geben-breakpoint-list-mode-help)
    map)
  "Keymap for `geben-breakpoint-list-mode'")

(defun geben-breakpoint-list-mode (session)
  "Major mode for GEBEN's breakpoint list.
The buffer commands are:
\\{geben-breakpoint-list-mode-map}"
  (unless (eq major-mode 'geben-breakpoint-list-mode)
    (kill-all-local-variables)
    (use-local-map geben-breakpoint-list-mode-map)
    (setq major-mode 'geben-breakpoint-list-mode)
    (setq mode-name "GEBEN breakpoints")
    (set (make-local-variable 'revert-buffer-function)
         (lambda (a b) nil))
    (and (fboundp 'font-lock-defontify)
         (add-hook 'change-major-mode-hook 'font-lock-defontify nil t))
    (setq buffer-read-only t)
    (buffer-disable-undo)
    (if (fboundp 'run-mode-hooks)
        (run-mode-hooks 'geben-breakpoint-list-mode-hook)
      (run-hooks 'geben-breakpoint-list-mode-hook)))
  (set (make-local-variable 'geben-current-session) session))

(defun geben-breakpoint-list-mark-delete ()
  "Add deletion mark."
  (interactive)
  (when (eq major-mode 'geben-breakpoint-list-mode)
    (let ((buffer-read-only nil))
      (beginning-of-line)
      (delete-char 1)
      (insert ?D)
      (forward-line 1))))

(defun geben-breakpoint-list-unmark ()
  "Remove deletion mark."
  (interactive)
  (when (eq major-mode 'geben-breakpoint-list-mode)
    (let ((buffer-read-only nil))
      (beginning-of-line)
      (delete-char 1)
      (insert " ")
      (forward-line 1))))

(defun geben-breakpoint-list-execute ()
  "Execute breakpoint deletion."
  (interactive)
  (when (eq major-mode 'geben-breakpoint-list-mode)
    (geben-with-current-session session
      (let (candidates)
        (save-excursion
          (goto-char (point-min))
          (let ((buffer-read-only nil))
            (while (re-search-forward "^D" nil t)
              (add-to-list 'candidates (get-text-property (point) 'geben-bp)))))
        (geben-breakpoint-remove session candidates)
        (when candidates
          (geben-breakpoint-list-display session))))))

(defun geben-breakpoint-list-mode-goto (&optional event)
  "Move to the set point of the selected breakpoint."
  (interactive (list last-nonmenu-event))
  (when (eq major-mode 'geben-breakpoint-list-mode)
    (geben-with-current-session session
      (let ((bp
             (if (or (null event)
                     (not (listp event)))
                 ;; Actually `event-end' works correctly with a nil argument as
                 ;; well, so we could dispense with this test, but let's not
                 ;; rely on this undocumented behavior.
                 (get-text-property (point) 'geben-bp)
               (with-current-buffer (window-buffer (posn-window (event-end event)))
                 (save-excursion
                   (goto-char (posn-point (event-end event)))
                   (get-text-property (point) 'geben-bp)))))
            same-window-buffer-names
            same-window-regexps)
        (let ((fileuri (plist-get bp :fileuri))
              (lineno (plist-get bp :lineno)))
          (and fileuri lineno
               (geben-session-cursor-update session fileuri lineno)))))))

(defun geben-breakpoint-list-mode-help ()
  "Display description and key bindings of `geben-breakpoint-list-mode'."
  (interactive)
  (describe-function 'geben-breakpoint-list-mode))

(defun geben-breakpoint-list-refresh (&optional force)
  "Display breakpoint list.
The breakpoint list buffer is under `geben-breakpoint-list-mode'.
Key mapping and other information is described its help page."
  (interactive)
  (geben-with-current-session session
    (when (and (geben-session-active-p session)
               (or force
                   (geben-session-buffer-visible-p session
                                                   geben-breakpoint-list-buffer-name)))
      (geben-dbgp-sequence
          (geben-dbgp-send-command session "breakpoint_list")
        (lambda (session cmd msg err)
          (geben-breakpoint-recreate session cmd msg err)
          (geben-breakpoint-list-display session))))))

(defun geben-breakpoint-recreate (session cmd msg err)
  "Create breakpoint objects according to the result of `breakpoint_list'."
  (unless err
    (dolist (msg-bp (xml-get-children msg 'breakpoint))
      (let* ((id (xml-get-attribute-or-nil msg-bp 'id))
             (bp (geben-session-breakpoint-find session id)))
        (unless bp
          (let* ((type (intern-soft (concat ":" (xml-get-attribute msg-bp 'type))))
                 (fileuri (xml-get-attribute-or-nil msg-bp 'filename))
                 (lineno (or (xml-get-attribute-or-nil msg-bp 'lineno)
                             (xml-get-attribute-or-nil msg-bp 'line)))
                 (function (xml-get-attribute-or-nil msg-bp 'function))
                 (class (xml-get-attribute-or-nil msg-bp 'class))
                 (method function)
                 (exception (xml-get-attribute-or-nil msg-bp 'exception))
                 (expression (xml-get-attribute-or-nil msg-bp 'expression))
                 (state (xml-get-attribute-or-nil msg-bp 'state))
                 (local-path (and fileuri
                                  (or (geben-session-source-local-path session fileuri)
                                      (geben-source-local-path session fileuri)))))
            (when (stringp lineno)
              (setq lineno (string-to-number lineno))
              (when (floatp lineno) ;; debugger engine may return invalid number.
                (setq lineno 1)))
            (when class
              (setq function (format "%s::%s" (or function "") class)))
            (when expression
              (setq expression (base64-decode-string expression)))
            (geben-session-breakpoint-add
             session
             (setq bp (geben-bp-make session type
                                     :id id
                                     :fileuri fileuri
                                     :lineno lineno
                                     :class class
                                     :method method
                                     :function function
                                     :exception exception
                                     :expression expression
                                     :state state
                                     :local-path local-path)))))
        (when bp
          (plist-put bp :hit-count (string-to-number (xml-get-attribute msg-bp 'hit_count)))
          (plist-put bp :hit-value (string-to-number (xml-get-attribute msg-bp 'hit_value))))))))

(defun geben-breakpoint-list-display (session)
  (let ((buf (geben-session-buffer session geben-breakpoint-list-buffer-name))
        (breakpoints (geben-breakpoint-list (geben-session-breakpoint session)))
        pos)
    (with-current-buffer buf
      (geben-breakpoint-list-mode session)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (if (or (not (listp breakpoints))
                (zerop (length breakpoints)))
            (insert "No breakpoints.\n")
          (setq breakpoints (sort (cl-copy-list breakpoints)
                                  #'geben-breakpoint-sort-pred))
          (mapc (lambda (bp)
                  (insert "  ")
                  (insert (format "%-11s"
                                  (or (cl-case (plist-get bp :type)
                                        (:line "Line")
                                        (:exception "Exception")
                                        (:call "Call")
                                        (:return "Return")
                                        (:conditional "Conditional")
                                        (:watch "Watch"))
                                      "Unknown")))
                  (if (geben-session-active-p session)
                      (insert (format "%2s/%-2s  "
                                      (or (plist-get bp :hit-count) "?")
                                      (let ((hit-value (plist-get bp :hit-value)))
                                        (cond
                                         ((null hit-value) "?")
                                         ((zerop hit-value) "*")
                                         (t hit-value)))))
                    (insert " "))
                  (when (plist-get bp :function)
                    (insert (propertize (plist-get bp :function)
                                        'face 'geben-breakpoint-function))
                    (insert " "))
                  (when (plist-get bp :exception)
                    (insert (propertize (plist-get bp :exception)
                                        'face 'geben-breakpoint-function))
                    (insert " "))
                  (when (plist-get bp :expression)
                    (insert (format "\"%s\" " (plist-get bp :expression))))
                  (when (plist-get bp :fileuri)
                    (insert (format "%s:%s"
                                    (propertize (plist-get bp :fileuri)
                                                'face 'geben-breakpoint-fileuri)
                                    (propertize (format "%s" (or (plist-get bp :lineno) "*"))
                                                'face 'geben-breakpoint-lineno))))
                  (insert "\n")
                  (put-text-property (save-excursion (forward-line -1) (point))
                                     (point)
                                     'geben-bp bp))
                breakpoints))
        (setq header-line-format
              (concat "  Type        "
                      (if (geben-session-active-p session) "Hits  " "")
                      "Property"))
        (goto-char (point-min))))
    (save-selected-window
      (geben-dbgp-display-window buf))))

;; overlay

(defun geben-bp-overlay-setup (bp)
  "Create an overlay for a breakpoint BP."
  (geben-bp-finalize bp)
  (let* ((local-path (plist-get bp :local-path))
         (overlay (and (stringp local-path)
                       (find-buffer-visiting local-path)
                       (geben-overlay-make-line (plist-get bp :lineno)
                                                (find-buffer-visiting local-path)))))
    (when overlay
      (overlay-put overlay 'face 'geben-breakpoint-face)
      (overlay-put overlay 'evaporate t)
      (overlay-put overlay 'bp bp)
      (overlay-put overlay 'modification-hooks '(geben-bp-overlay-modified))
      (overlay-put overlay 'insert-in-front-hooks '(geben-bp-overlay-inserted-in-front))
      (plist-put bp :overlay overlay)))
  bp)

(defun geben-bp-overlay-hide (session)
  "Hide breakpoint overlays."
  (mapc (lambda (bp)
          (let ((overlay (plist-get bp :overlay)))
            (and (overlayp overlay)
                 (overlay-livep overlay)
                 (overlay-put overlay 'face nil))))
        (geben-breakpoint-list (geben-session-breakpoint session))))

(defun geben-bp-overlay-modified (overlay afterp beg end &optional len)
  "A callback function invoked when inside of an overlay is modified.
With this callback GEBEN tracks displacements of line breakpoints."
  (when afterp
    (save-excursion
      (save-restriction
        (widen)
        (let* ((lineno-from (progn (goto-char (overlay-start overlay))
                                   (geben-what-line)))
               (lineno-to (progn (goto-char (overlay-end overlay))
                                 (geben-what-line)))
               (lineno lineno-from))
          (goto-line lineno)
          (while (and (looking-at "[ \t]*$")
                      (< lineno lineno-to))
            (forward-line)
            (cl-incf lineno))
          (if (< lineno-from lineno)
              (plist-put (overlay-get overlay 'bp) :lineno lineno))
          (goto-line lineno)
          (beginning-of-line)
          (move-overlay overlay (point) (save-excursion
                                          (forward-line)
                                          (point))))))))

(defun geben-bp-overlay-inserted-in-front (overlay afterp beg end &optional len)
  "A callback function invoked when text in front of an overlay is modified.
With this callback GEBEN tracks displacements of line breakpoints."
  (if afterp
      (save-excursion
        (goto-line (progn (goto-char (overlay-start overlay))
                          (geben-what-line)))
        (move-overlay overlay (point) (save-excursion
                                        (forward-line)
                                        (point))))))

(defun geben-bp-overlay-restore (session buf)
  "A callback function invoked when emacs visits a new file.
GEBEN may place overlay markers if there are line breakpoints in
the file."
  (mapc (lambda (bp)
          (and (plist-get bp :lineno)
               (eq buf (find-buffer-visiting (or (plist-get bp :local-path)
                                                 "")))
               (geben-bp-overlay-setup bp)))
        (geben-breakpoint-list (geben-session-breakpoint session))))

(defun geben-session-breakpoint-init (session)
  (setf (geben-session-breakpoint session) (geben-breakpoint-make))
  (geben-session-breakpoint-storage-restore session))

(add-hook 'geben-session-enter-hook #'geben-session-breakpoint-init)

(defun geben-session-breakpoint-release (session)
  (when geben-show-breakpoints-debugging-only
    (geben-bp-overlay-hide session)))

(add-hook 'geben-session-exit-hook #'geben-session-breakpoint-release)

(defun geben-dbgp-breakpoint-store-types (session cmd msg err)
  (when (equal "1" (xml-get-attribute msg 'supported))
    (let ((types (mapcar
                  (lambda (type)
                    (intern (concat ":" type)))
                  (split-string (or (car (xml-node-children msg))
                                    "")
                                " "))))
      (if (geben-session-xdebug-p session)
          ;; Xdebug 2.0.3 supports the following types but they aren't
          ;; included in the response. Push them in the list manually.
          (setq types (append types '(:exception :conditional))))
      (unless types
        ;; Some debugger engines are buggy;
        ;; they don't return breakpoint types correctly.
        ;; To them put all of types to the list.
        (setq types '(:line :call :return :exception :conditional :watch)))
      (setf (geben-breakpoint-types (geben-session-breakpoint session)) types))))

(add-hook 'geben-source-visit-hook #'geben-bp-overlay-restore)

;;; breakpoint_set

(defun geben-dbgp-command-breakpoint-set (session bp)
  "Send \`breakpoint_set\' command."
  (if (not (geben-session-active-p session))
      (geben-session-breakpoint-add session bp)
    (let ((obp (geben-session-breakpoint-find session bp)))
      (if (and obp
               (plist-get obp :id))
          (geben-dbgp-send-command session "breakpoint_update"
                                   (cons "-d" (plist-get obp :id))
                                   (cons "-h" (or (plist-get bp :hit-value)
                                                  0))
                                   (cons "-o" ">="))
        (let ((params
               (remove nil
                       (list
                        (cons "-t"
                              (substring (symbol-name (plist-get bp :type)) 1))
                        (and (plist-get bp :fileuri)
                             (cons "-f" (plist-get bp :fileuri)))
                        (and (plist-get bp :lineno)
                             (cons "-n" (plist-get bp :lineno)))
                        (and (plist-get bp :class)
                             (geben-session-xdebug-p session)
                             (cons "-a" (plist-get bp :class)))
                        (and (plist-get bp :function)
                             (if (and (geben-session-xdebug-p session)
                                      (plist-get bp :method))
                                 (cons "-m" (plist-get bp :method))
                               (cons "-m" (plist-get bp :function))))
                        (and (plist-get bp :exception)
                             (cons "-x" (plist-get bp :exception)))
                        (cons "-h" (or (plist-get bp :hit-value) 0))
                        (cons "-o" ">=")
                        (cons "-s" (or (plist-get bp :state)
                                       "enabled"))
                        (cons "-r" (if (plist-get bp :run-once) 1 0))
                        (and (plist-get bp :expression)
                             (cons "--"
                                   (base64-encode-string
                                    (plist-get bp :expression))))))))
          (when params
            (apply 'geben-dbgp-send-command session "breakpoint_set" params)))))))

(defun geben-dbgp-response-breakpoint-set (session cmd msg)
  "A response message handler for \`breakpoint_set\' command."
  (unless (eq (geben-cmd-param-get cmd "-r") 1) ; unless :run-once is set
    (let* ((type (intern (concat ":" (geben-cmd-param-get cmd "-t"))))
           (id (xml-get-attribute-or-nil msg 'id))
           (fileuri (geben-cmd-param-get cmd "-f"))
           (lineno (geben-cmd-param-get cmd "-n"))
           (function (geben-cmd-param-get cmd "-m"))
           (class (geben-cmd-param-get cmd "-a"))
           (method function)
           (exception (geben-cmd-param-get cmd "-x"))
           (expression (geben-cmd-param-get cmd "--"))
           (hit-value (geben-cmd-param-get cmd "-h"))
           (state (geben-cmd-param-get cmd "-s"))
           (local-path (and fileuri
                            (or (geben-session-source-local-path session fileuri)
                                (geben-source-local-path session fileuri))))
           bp)
      (when expression
        (setq expression (base64-decode-string expression)))
      (geben-session-breakpoint-add session
                                    (setq bp (geben-bp-make session type
                                                            :id id
                                                            :fileuri fileuri
                                                            :lineno lineno
                                                            :class class
                                                            :method method
                                                            :function function
                                                            :exception exception
                                                            :expression expression
                                                            :hit-value hit-value
                                                            :local-path local-path
                                                            :state state))))
    (geben-breakpoint-list-refresh)))

(defun geben-dbgp-response-breakpoint-update (session cmd msg)
  "A response message handler for `breakpoint_update' command."
  (let* ((id (geben-cmd-param-get cmd "-d"))
         (bp (geben-session-breakpoint-find session id)))
    (when bp
      (plist-put bp :hit-value (geben-cmd-param-get cmd "-h"))
      (geben-breakpoint-list-refresh))))

;;; breakpoint_remove

(defun geben-dbgp-command-breakpoint-remove (session bid)
  "Send `breakpoint_remove' command."
  (if (geben-session-active-p session)
      (geben-dbgp-sequence-bind (bid)
        (geben-dbgp-send-command session "breakpoint_remove" (cons "-d" bid))
        (lambda (session cmd msg err)
          (when (dbgp-xml-get-error-message msg)
            ;; remove a stray breakpoint from hash table.
            (geben-session-breakpoint-remove session bid)
            (geben-breakpoint-list-refresh))))
    (geben-session-breakpoint-remove session bid)))

(defun geben-dbgp-response-breakpoint-remove (session cmd msg)
  "A response message handler for \`breakpoint_remove\' command."
  (let* ((id (geben-cmd-param-get cmd "-d"))
         (bp (geben-session-breakpoint-find session id)))
    (geben-session-breakpoint-remove session id)
    (geben-breakpoint-list-refresh)))

(defun geben-dbgp-command-breakpoint-list (session)
  "Send `breakpoint_list' command."
  (geben-dbgp-send-command session "breakpoint_list"))

(defun geben-dbgp-response-breakpoint-list (session cmd msg)
  "A response message handler for \`breakpoint_list\' command."
  t)

(defun geben-dbgp-breakpoint-list-refresh (session)
  (geben-breakpoint-list-refresh))



;;==============================================================
;; context
;;==============================================================

(defface geben-context-category-face
  '((((class color))
     :background "purple"
     :foreground "white"
     :bold t))
  "Face used to highlight context category name."
  :group 'geben-highlighting-faces)

(defface geben-context-variable-face
  '((t :inherit 'font-lock-variable-name-face))
  "Face used to highlight variable name."
  :group 'geben-highlighting-faces)

(defface geben-context-type-face
  '((t :inherit 'font-lock-type-face))
  "Face used to highlight type name."
  :group 'geben-highlighting-faces)

(defface geben-context-class-face
  '((t :inherit 'font-lock-constant-face))
  "Face used to highlight type name."
  :group 'geben-highlighting-faces)

(defface geben-context-string-face
  '((t :inherit 'font-lock-string-face))
  "Face used to highlight string value."
  :group 'geben-highlighting-faces)

(defface geben-context-constant-face
  '((t :inherit 'font-lock-constant-face))
  "Face used to highlight numeric value."
  :group 'geben-highlighting-faces)

(cl-defstruct (geben-context
               (:constructor nil)
               (:constructor geben-context-make))
  names	   ; context names alist(KEY: context name, VALUE: context id)
  tid  ; transaction id to which the current context variables belong.
  variables			;
  expanded-variables		; context variables in expanded state.
  (depth 0)
  )

(defvar geben-context-where "")
(defvar geben-context-loading nil)
(defvar geben-context-property-tree-fill-children-hook 'geben-context-tree-children-fill)

(defvar geben-expanded-context-variables '("Locals"))
(add-hook 'tree-widget-after-toggle-functions 'geben-tree-widget-notify)

(add-hook 'geben-after-eval-expression 'geben-context-mode-refresh)

(defun geben-session-context-init (session)
  (setf (geben-session-context session) (geben-context-make)))
(add-hook 'geben-session-enter-hook #'geben-session-context-init)

;; context list buffer

(defsubst geben-session-context-buffer (session)
  (let ((buf (geben-session-buffer session geben-context-buffer-name)))
    (with-current-buffer buf
      (geben-context-mode session))
    buf))

(defsubst geben-session-context-buffer-get (session)
  (geben-session-buffer-get session geben-context-buffer-name))

(defsubst geben-session-context-buffer-live-p (session)
  (geben-session-buffer-live-p session geben-context-buffer-name))

(defsubst geben-session-context-buffer-visible-p (session)
  (geben-session-buffer-visible-p session geben-context-buffer-name))

;;

(defsubst geben-session-context-tid (session)
  (geben-context-tid (geben-session-context session)))

(defsubst geben-session-context-names (session)
  (geben-context-names (geben-session-context session)))

(defsubst geben-session-context-depth (session)
  (geben-context-depth (geben-session-context session)))

;; context list accessors

(defsubst geben-session-context-list (session cid)
  "Get context list for the context id CID."
  (assq cid
        (geben-context-variables
         (geben-session-context session))))

(defsubst geben-session-context-list-old (session cid)
  "Get previous context list for the context id CID."
  (cdr (assq 'old (geben-session-context-list session cid))))

(defsubst geben-session-context-list-new (session cid)
  "Get the current context list for the context id CID."
  (cdr (assq 'new (geben-session-context-list session cid))))

(defsubst geben-session-context-list-update (session cid list)
  "Update the current context list for the context id CID with LIST."
  (let* ((clist (geben-session-context-list session cid))
         (old (assq 'new clist)))
    (setcdr clist (list (cons 'old (cdr old))
                        (cons 'new list)))))

;; context property list accessors

(defsubst geben-context-property-has-children (property)
  "Check whether PROPERTY has any children."
  (equal "1" (xml-get-attribute-or-nil property 'children)))

(defsubst geben-context-property-format-bool (value)
  "Format VALUE in the debuggee language expression."
  (let ((bool (if (equal "0" value) nil t)))
    (if bool "true" "false")))

(defsubst geben-context-property-format-array-name (property)
  "Format array element name in the debuggee language expression."
  (format "%s[%s]"
          (propertize (xml-get-attribute property 'name)
                      'face 'geben-context-variable-face)
          (propertize (xml-get-attribute property 'numchildren)
                      'face 'geben-context-constant-face)))

(defsubst geben-context-property-attribute (property sym)
  "Get attribute SYM from PROPERTY."
  ;; DBGp specs specifies property attributes of context_get and
  ;; property_get commands. But some debugger engines have values not
  ;; as attributes but child elements."
  (let ((node (car (xml-get-children property sym))))
    (if (consp node)
        (geben-dbgp-decode-string (xml-node-children node)
                                  (xml-get-attribute node 'encoding)
                                  'utf-8)
      (xml-get-attribute property sym))))

(defsubst geben-context-property-name (property)
  "Get name attribute value from PROPERTY."
  (geben-context-property-attribute property 'name))

(defsubst geben-context-property-fullname (property)
  "Get fullname attribute value from PROPERTY."
  (geben-context-property-attribute property 'fullname))

(defsubst geben-context-property-value (property)
  "Get value from PROPERTY."
  (let ((node (car (xml-get-children property 'value))))
    (if (consp node)
        (geben-dbgp-decode-string (xml-node-children node)
                                  (xml-get-attribute node 'encoding)
                                  'utf-8)
      (geben-dbgp-decode-string (xml-node-children property)
                                (xml-get-attribute property 'encoding)
                                'utf-8))))

(defun geben-context-property-typeinfo (property)
  "Get type information of PROPERTY to display it in the context buffer."
  (let ((type (and (xml-get-attribute-or-nil property 'type)
                   (intern (xml-get-attribute-or-nil property 'type))))
        typeinfo)
    (setq typeinfo
          (cond
           ((null type) nil)
           ((member type '(int float))
            (list :type type
                  :type-visiblep nil
                  :value-face 'geben-context-constant-face))
           ((eq type 'bool)
            (list :type type
                  :type-visiblep nil
                  :value-face 'geben-context-constant-face
                  :value-formatter 'geben-context-property-format-bool))
           ((eq type 'string)
            (list :type type
                  :type-visiblep nil
                  :value-face 'geben-context-string-face))
           ((member type '(array hash))
            (list :type type
                  :type-visiblep nil
                  :name-formatter 'geben-context-property-format-array-name
                  :value-face 'default
                  :value-formatter (lambda (value) "")))
           ((eq type 'null)
            (list :type type
                  :type-visiblep nil
                  :value-face 'geben-context-constant-face
                  :value-formatter (lambda (value) "null")))
           ((eq type 'resource)
            (list :type type
                  :type-visiblep t
                  :value-face 'geben-context-constant-face))
           ((eq type 'object)
            (list :type (if (xml-get-attribute-or-nil property 'classname)
                            (intern (xml-get-attribute-or-nil property 'classname))
                          type)
                  :type-visiblep t
                  :type-face 'geben-context-class-face
                  :value-face 'default))
           ((eq type 'uninitialized)
            (list :type 'undef
                  :type-visiblep t
                  :type-face 'geben-context-type-face
                  :value-face 'default))
           (t
            (list :type type
                  :type-visiblep t
                  :type-face 'geben-context-type-face
                  :value-face 'default))))
    typeinfo))

;;--------------------------------------------------------------
;; context property tree widget
;;--------------------------------------------------------------

(defun geben-context-property-tree-open (tree)
  "Expand TREE."
  (let ((marker (widget-get tree :from)))
    (when (markerp marker)
      (with-current-buffer (marker-buffer marker)
        (goto-char marker)
        (call-interactively 'widget-button-press)
        (unless (widget-get tree :open)
          (call-interactively 'widget-button-press))))))

(defun geben-context-property-tree-expand-p (tree)
  "A tree widget callback function to indicate whether TREE is able to expand."
  (or (geben-context-property-tree-has-complete-children tree)
      (and (run-hook-with-args 'geben-context-property-tree-fill-children-hook
                               tree)
           nil)))

(defun geben-context-property-tree-expand (tree)
  "A tree widget callback function to create child list of TREE."
  (mapcar #'geben-context-property-tree-create-node
          (xml-get-children (widget-get tree :property) 'property)))

(defun geben-context-property-tree-has-complete-children (tree)
  "Determine whether TREE has complete child nodes.
Child nodes can be short for :property property of TREE."
  (let* ((property (widget-get tree :property))
         (children (xml-get-children property 'property))
         (numchildren (and children
                           (string-to-number (xml-get-attribute property 'numchildren)))))
    (and children
         (<= numchildren (length children)))))

(defun geben-context-property-tree-create-node (property)
  "Create nodes which represent PROPERTY."
  (let* ((typeinfo (geben-context-property-typeinfo property))
         (value (geben-context-property-value property))
         tag)
    (let ((formatter (plist-get typeinfo :name-formatter)))
      (setq tag
            (if formatter
                (funcall formatter property)
              (propertize (geben-context-property-name property)
                          'face 'geben-context-variable-face))))
    (when (plist-get typeinfo :type-visiblep)
      (setq tag (concat tag
                        (format "(%s)" (propertize
                                        (symbol-name (plist-get typeinfo :type))
                                        'face (plist-get typeinfo :type-face))))))
    (let ((formatter (plist-get typeinfo :value-formatter)))
      (when (or value formatter)
        (setq tag (format "%-32s %s" tag
                          (propertize (if formatter
                                          (funcall formatter value)
                                        value)
                                      'face (plist-get typeinfo :value-face))))))
    (if (geben-context-property-has-children property)
        (list 'tree-widget
              :tag tag
              :open (member (or (geben-tree-var-identifier property) tag) geben-expanded-context-variables)
              :property property
              :expander 'geben-context-property-tree-expand
              :expander-p 'geben-context-property-tree-expand-p)
      (list 'item :tag (concat "   " tag)))))

(defun geben-context-property-tree-context-id (tree)
  "Get context id to which TREE belongs."
  (when tree
    (let ((cid (widget-get tree :context-id)))
      (or cid
          (geben-context-property-tree-context-id (widget-get tree :parent))))))

;;--------------------------------------------------------------
;; context functions
;;--------------------------------------------------------------

(defun geben-context-list-fetch (session callback)
  "Fetch context variables for a SESSION from debuggee server.
After fetching it calls CALLBACK function."
  (let ((context (geben-session-context session)))
    (when (geben-context-names context)
      (unless (geben-context-variables context)
        (setf (geben-context-variables context)
              (mapcar (lambda (context)
                        (list (cdr context)))
                      (geben-context-names context))))
      ;; Remain the current tid.
      ;; It is possible that the current context proceeds by step_in or
      ;; other continuous commands while retrieving variables.
      ;; To avoid mixing variables with multi context, remain something at here,
      ;; tid, and check the value in the retrieving process.
      (setf (geben-context-tid context) (geben-session-tid session))
      (geben-context-list-fetch-loop session
                                     (geben-context-tid context)
                                     (geben-context-depth context)
                                     (mapcar (lambda (context)
                                               (cdr context))
                                             (geben-context-names context))
                                     callback))))

(defun geben-context-list-fetch-loop (session tid-save depth context-id-list callback)
  (let ((buf (geben-session-context-buffer-get session)))
    (when buf
      (with-current-buffer buf
        (setq geben-context-loading t))
      (geben-dbgp-sequence-bind (tid-save depth context-id-list callback)
        (geben-dbgp-command-context-get session (car context-id-list) depth)
        (lambda (session cmd msg err)
          (when (and (not err)
                     (eq tid-save (geben-session-context-tid session))
                     (geben-session-context-buffer-live-p session))
            (geben-session-context-list-update session
                                               (geben-cmd-param-get cmd "-c")
                                               (xml-get-children msg 'property))
            (if (cdr context-id-list)
                (geben-context-list-fetch-loop session tid-save depth
                                               (cdr context-id-list) callback)
              (geben-context-fill-buffer session)
              (with-current-buffer (geben-session-context-buffer-get session)
                (setq geben-context-loading nil))
              (funcall callback session))))))))

(defun geben-context-fill-buffer (session)
  "Fill the context buffer with locally stored context list.
Example: Locals, Superglobals"
  (let ((buf (geben-session-context-buffer-get session)))
    (when buf
      (with-current-buffer buf
        (let ((inhibit-read-only t)
              (inhibit-modification-hooks t))
          (widen)
          (erase-buffer)
          (dolist (context-name (geben-session-context-names session))
            (let ((new (geben-session-context-list-new session (cdr context-name))))
              (apply 'widget-create
                     'tree-widget
                     :tag (car context-name)
                     :context-id (cdr context-name)
                     :open (member (car context-name) geben-expanded-context-variables)
                     (mapcar #'geben-context-property-tree-create-node new))))
          (widget-setup))
        (goto-char (point-min))))))

(defun geben-context-tree-children-fill (tree &optional tid-save)
  (geben-with-current-session session
    (let ((tid-save (or tid-save
                        (geben-session-context-tid session)))
          (completed (geben-context-property-tree-has-complete-children tree))
          (buf (geben-session-context-buffer-get session)))
      (when (and (buffer-live-p buf)
                 (eq tid-save (geben-session-context-tid session)))
        (with-current-buffer buf
          (setq geben-context-loading (not completed)))
        (if completed
            (geben-context-property-tree-open tree)
          (geben-context-tree-children-fill-1 session tree tid-save))))))

(defun geben-context-tree-children-fill-1 (session tree tid-save)
  (let* ((property (widget-get tree :property))
         (children (xml-get-children property 'property)))
    (with-current-buffer (geben-session-context-buffer-get session)
      ;; -- comment on :property-page property --
      ;; debugger engine may lack of PAGESIZE in property message(bug).
      ;; so the following code doesn't rely on PAGESIZE but uses own
      ;; :property-page widget property.
      (let* ((nextpage (if (widget-get tree :property-page)
                           (1+ (widget-get tree :property-page))
                         (if children 1 0)))
             (args (list :depth (geben-session-context-depth session)
                         :context-id (geben-context-property-tree-context-id tree)
                         :name (geben-context-property-fullname property)
                         :page nextpage)))
        (widget-put tree :property-page nextpage)
        (when (xml-get-attribute-or-nil property 'key)
          (plist-put args :key (xml-get-attribute-or-nil property 'key)))
        (geben-dbgp-sequence-bind (tree tid-save)
          (geben-dbgp-command-property-get session args)
          (lambda (session cmd msg err)
            (unless err
              (geben-context-tree-children-append session
                                                  tid-save
                                                  tree
                                                  (car (xml-get-children msg 'property)))
              (geben-context-tree-children-fill tree
                                                tid-save))))))))

(defun geben-tree-var-identifier (property)
  (geben-context-property-attribute property 'fullname))

(defun geben-tree-widget-notify (widget-tree)
  "Takes WIDGET-TREE and gathers the address of the variable just expanded/collapsed,
 if it was expanded it gets added to `geben-expanded-context-variables' and if it was collapsed
 it gets removed."
  (let* ((property (widget-get widget-tree :property))
         (var-address (if property
                          (geben-tree-var-identifier property)
                        (widget-get widget-tree :tag))))
    (if (widget-get widget-tree :open)
        (add-to-list 'geben-expanded-context-variables var-address)
      (setq geben-expanded-context-variables (cl-remove var-address geben-expanded-context-variables :test 'string-equal)))))

(defun geben-context-tree-children-append (session tid-save tree property)
  (if (eq tid-save (geben-session-context-tid session))
      (let ((tree-prop (widget-get tree :property)))
        (nconc (or (cddr tree-prop)
                   tree-prop)
               (cddr property)))))

(defun geben-context-list-refresh (session depth &optional force)
  (when (and (geben-session-active-p session)
             (or force
                 (geben-session-context-buffer-visible-p session)))
    (geben-context-list-display session depth (not force))))

(defun geben-context-list-display (session depth &optional no-select)
  "Display context variables in the context buffer."
  (unless (geben-session-active-p session)
    (error "GEBEN is out of debugging session."))
  (when (or (< depth 0)
            (< (length (geben-session-stack session)) (1+ depth)))
    (error "GEBEN context display: invalid depth: %S" depth))
  (setf (geben-context-depth (geben-session-context session)) depth)
  (let ((buf (geben-session-context-buffer session)))
    (with-current-buffer buf
      (setq geben-context-where
            (xml-get-attribute (nth depth (geben-session-stack session))
                               'where)))
    (unless no-select
      (geben-dbgp-display-window buf))
    (geben-context-list-fetch session
                              (geben-lexical-bind (buf no-select)
                                (lambda (session)
                                  (and (buffer-live-p buf)
                                       (not no-select)
                                       (geben-dbgp-display-window buf)))))))

;;--------------------------------------------------------------
;; context mode
;;--------------------------------------------------------------

(defcustom geben-context-mode-hook nil
  "*Hook running at when GEBEN's context buffer is initialized."
  :group 'geben
  :type 'hook)

(defvar geben-context-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\t" 'widget-forward)
    (define-key map "S-\t" 'widget-backward)
    ;;(define-key map "\C-m" 'geben-context-mode-expand)
    ;;(define-key map "e" 'geben-context-mode-edit)
    (define-key map "r" 'geben-context-mode-refresh)
    (define-key map "q" 'geben-quit-window)
    (define-key map "p" 'widget-backward)
    (define-key map "n" 'widget-forward)
    (define-key map "?" 'geben-context-mode-help)
    map)
  "Keymap for `geben-context-mode'")

(defun geben-context-mode (session)
  "Major mode for GEBEN's context output.
The buffer commands are:
\\{geben-context-mode-map}"
  (interactive)
  (unless (eq major-mode 'geben-context-mode)
    (kill-all-local-variables)
    (use-local-map geben-context-mode-map)
    (setq major-mode 'geben-context-mode)
    (setq mode-name "GEBEN context")
    (set (make-local-variable 'revert-buffer-function)
         (lambda (a b) nil))
    (and (fboundp 'font-lock-defontify)
         (add-hook 'change-major-mode-hook 'font-lock-defontify nil t))
    (if (fboundp 'run-mode-hooks)
        (run-mode-hooks 'geben-context-mode-hook)
      (run-hooks 'geben-context-mode-hook))
    (buffer-disable-undo)
    (set (make-local-variable 'geben-context-where) "")
    (set (make-local-variable 'geben-context-loading) nil)
    (set (make-local-variable 'tree-widget-theme) "geben")
    (setq header-line-format
          (list
           "Where: "
           'geben-context-where
           "   "
           '(geben-context-loading "(loading...)")
           ))
    (setq buffer-read-only t))
  (set (make-local-variable 'geben-current-session) session))

(defun geben-context-mode-refresh (&optional force)
  "Refresh the context buffer."
  (interactive)
  (geben-with-current-session session
    (geben-context-list-refresh session
                                (geben-session-context-depth session)
                                force)))

(defun geben-context-mode-help ()
  "Display description and key bindings of `geben-context-mode'."
  (interactive)
  (describe-function 'geben-context-mode))

;; context

(defun geben-dbgp-command-context-names (session &optional depth)
  (geben-dbgp-send-command session "context_names"
                           (and (numberp depth)
                                (cons "-d" depth))))

(defun geben-dbgp-response-context-names (session cmd msg)
  (setf (geben-context-names (geben-session-context session))
        (mapcar (lambda (context)
                  (let ((name (xml-get-attribute context 'name))
                        (id (xml-get-attribute context 'id)))
                    (cons name (string-to-number id))))
                (xml-get-children msg 'context))))

;; context

(defun geben-dbgp-command-context-get (session context-id &optional depth)
  (geben-dbgp-send-command session "context_get"
                           (cons "-c" context-id)
                           (and depth
                                (cons "-d" depth))))

;; property

(defun geben-dbgp-command-property-get (session &rest args)
  (apply 'geben-dbgp-send-command session "property_get"
         (let* ((initmsg (geben-session-initmsg session))
                (lang (xml-get-attribute-or-nil initmsg 'language)))
           (mapcar (lambda (key)
                     (let ((arg (plist-get (car args) key)))
                       (when arg
                         (cons (geben-cmd-param-for key)
                               (if (and (eq key :name)
                                        (not (string= lang "PHP")))
                                   (geben-dbgp-escape-param-arg arg)
                                 arg)))))
                   '(:depth :context-id :name :max-data-size :type :page :key :address)))))

(defun geben-dbgp-escape-param-arg (param)
  (if (stringp param)
      (concat
       "\""
       (replace-regexp-in-string
        "*" "\\\\*"
        (replace-regexp-in-string
         "\"" "\\\\\""
         param))
       "\"")
    param))

;;==============================================================
;; stack
;;==============================================================

;; backtrace

(defface geben-backtrace-fileuri
  '((((class color))
     (:foreground "green" :weight bold))
    (t (:weight bold)))
  "Face used to highlight fileuri in backtrace buffer."
  :group 'geben-highlighting-faces)

(defface geben-backtrace-lineno
  '((t :inherit font-lock-variable-name-face))
  "Face for displaying line numbers in backtrace buffer."
  :group 'geben-highlighting-faces)

(defcustom geben-backtrace-mode-hook '(hl-line-mode)
  "*Hook running at when GEBEN's backtrace buffer is initialized."
  :group 'geben
  :type 'hook)

(defun geben-backtrace-buffer (session)
  (let ((buf (get-buffer-create (geben-session-buffer session geben-backtrace-buffer-name))))
    (with-current-buffer buf
      (geben-backtrace-mode session))
    buf))

(defun geben-backtrace (session)
  "Display backtrace."
  (unless (geben-session-active-p session)
    (error "GEBEN is out of debugging session."))
  (with-current-buffer (geben-backtrace-buffer session)
    (let ((inhibit-read-only t)
          (stack (geben-session-stack session)))
      (erase-buffer)
      (dotimes (i (length stack))
        (let* ((stack (nth i stack))
               (fileuri (geben-source-fileuri-regularize (xml-get-attribute stack 'filename)))
               (lineno (xml-get-attribute stack 'lineno))
               (where (xml-get-attribute stack 'where))
               (level (xml-get-attribute stack 'level)))
          (insert (format "%s:%s %s\n"
                          (propertize fileuri 'face "geben-backtrace-fileuri")
                          (propertize lineno 'face "geben-backtrace-lineno")
                          where))
          (put-text-property (save-excursion (forward-line -1) (point))
                             (point)
                             'geben-stack-frame
                             (list :fileuri fileuri
                                   :lineno lineno
                                   :level (string-to-number level)))))
      (goto-char (point-min)))
    (geben-dbgp-display-window (geben-backtrace-buffer session))))

(defvar geben-backtrace-mode-map nil
  "Keymap for `geben-backtrace-mode'")
(unless geben-backtrace-mode-map
  (setq geben-backtrace-mode-map
        (let ((map (make-sparse-keymap)))
          (define-key map [mouse-2] 'geben-backtrace-mode-mouse-goto)
          (define-key map "\C-m" 'geben-backtrace-mode-goto)
          (define-key map "q" 'geben-quit-window)
          (define-key map "p" 'previous-line)
          (define-key map "n" 'next-line)
          (define-key map "v" 'geben-backtrace-mode-context)
          (define-key map "?" 'geben-backtrace-mode-help)
          map)))

(defun geben-backtrace-mode (session)
  "Major mode for GEBEN's backtrace output.
The buffer commands are:
\\{geben-backtrace-mode-map}"
  (interactive)
  (unless (eq 'geben-backtrace-mode major-mode)
    (kill-all-local-variables)
    (use-local-map geben-backtrace-mode-map)
    (setq major-mode 'geben-backtrace-mode)
    (setq mode-name "GEBEN backtrace")
    (set (make-local-variable 'revert-buffer-function)
         (lambda (a b) nil))
    (and (fboundp 'font-lock-defontify)
         (add-hook 'change-major-mode-hook 'font-lock-defontify nil t))
    (setq buffer-read-only t)
    (buffer-disable-undo)
    (if (fboundp 'run-mode-hooks)
        (run-mode-hooks 'geben-backtrace-mode-hook)
      (run-hooks 'geben-backtrace-mode-hook)))
  (set (make-local-variable 'geben-current-session) session))

(defalias 'geben-backtrace-mode-mouse-goto 'geben-backtrace-mode-goto)
(defun geben-backtrace-mode-goto (&optional event)
  (interactive (list last-nonmenu-event))
  (geben-with-current-session session
    (let ((stack-frame
           (if (or (null event)
                   (not (listp event)))
               ;; Actually `event-end' works correctly with a nil argument as
               ;; well, so we could dispense with this test, but let's not
               ;; rely on this undocumented behavior.
               (get-text-property (point) 'geben-stack-frame)
             (with-current-buffer (window-buffer (posn-window (event-end event)))
               (save-excursion
                 (goto-char (posn-point (event-end event)))
                 (get-text-property (point) 'geben-stack-frame)))))
          same-window-buffer-names
          same-window-regexps)
      (when stack-frame
        (geben-session-cursor-update session
                                     (plist-get stack-frame :fileuri)
                                     (plist-get stack-frame :lineno))
        (run-hook-with-args 'geben-dbgp-stack-update-hook
                            session (plist-get stack-frame :level))))))

(defun geben-backtrace-mode-help ()
  "Display description and key bindings of `geben-backtrace-mode'."
  (interactive)
  (describe-function 'geben-backtrace-mode))

(defvar geben-dbgp-stack-update-hook nil)

(defun geben-backtrace-mode-context ()
  (interactive)
  (geben-with-current-session session
    (let ((stack (get-text-property (point) 'geben-stack-frame)))
      (when stack
        (run-hook-with-args 'geben-dbgp-stack-update-hook
                            session (plist-get stack :level))))))

;;; stack_get

(defun geben-dbgp-command-stack-get (session)
  "Send \`stack_get\' command."
  (geben-dbgp-send-command session "stack_get"))

(defun geben-dbgp-stack-update (session)
  (geben-dbgp-sequence
      (geben-dbgp-command-stack-get session)
    (lambda (session cmd msg err)
      (unless err
        (setf (geben-session-stack session) (xml-get-children msg 'stack))
        (let* ((stack (car (xml-get-children msg 'stack)))
               (fileuri (xml-get-attribute-or-nil stack 'filename))
               (lineno (xml-get-attribute-or-nil stack 'lineno)))
          (and fileuri lineno
               (geben-session-cursor-update session fileuri lineno)))
        (run-hook-with-args 'geben-dbgp-stack-update-hook
                            session 0)))))


;;==============================================================
;; redirect
;;==============================================================

(defconst geben-redirect-combine-buffer-name "*GEBEN<%s> output*"
  "Name for the debuggee script's STDOUT and STDERR redirection buffer.")
(defconst geben-redirect-stdout-buffer-name "*GEBEN<%s> stdout*"
  "Name for the debuggee script's STDOUT redirection buffer.")
(defconst geben-redirect-stderr-buffer-name "*GEBEN<%s> stderr*"
  "Name for the debuggee script's STDERR redirection buffer.")

(cl-defstruct (geben-redirect
               (:constructor nil)
               (:constructor geben-redirect-make))
  (stdout :redirect)
  (stderr :redirect)
  (combine t)
  (coding-system 'utf-8))

(defcustom geben-dbgp-redirect-buffer-init-hook nil
  "*Hook running at when a redirection buffer is created."
  :group 'geben
  :type 'hook)

(defun geben-session-redirect-init (session)
  (setf (geben-session-redirect session) (geben-redirect-make))
  (dolist (type '(:stdout :stderr))
    (let ((buf (get-buffer (geben-session-redirect-buffer-name session type))))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (let ((inhibit-read-only t)
                (inhibit-modification-hooks t))
            (erase-buffer)))))))

(add-hook 'geben-session-enter-hook #'geben-session-redirect-init)

(defun geben-session-redirect-buffer (session type)
  (let ((bufname (geben-session-redirect-buffer-name session type)))
    (when bufname
      (or (get-buffer bufname)
          (with-current-buffer (get-buffer-create bufname)
            (unless (local-variable-p 'geben-dynamic-property-buffer-p)
              (set (make-local-variable 'geben-dynamic-property-buffer-p) t)
              (setq buffer-undo-list t)
              (run-hook-with-args 'geben-dbgp-redirect-buffer-init-hook (current-buffer)))
            (current-buffer))))))

(defun geben-session-redirect-buffer-name (session type)
  "Select buffer name for a redirection type."
  (let ((redirect (geben-session-redirect session)))
    (when (or (and (eq type :stdout)
                   (geben-redirect-stdout redirect))
              (and (eq type :stderr)
                   (geben-redirect-stderr redirect)))
      (geben-session-buffer-name session
                                 (cond
                                  ((geben-redirect-combine redirect)
                                   geben-redirect-combine-buffer-name)
                                  ((eq :stdout type)
                                   geben-redirect-stdout-buffer-name)
                                  (t
                                   geben-redirect-stderr-buffer-name))))))

(defun geben-session-redirect-buffer-existp (session)
  "Check whether any redirection buffer exists."
  (let (name)
    (or (and (setq name (geben-session-redirect-buffer-name session :stdout))
             (get-buffer name))
        (and (setq name (geben-session-redirect-buffer-name session :stderr))
             (get-buffer name)))))

(defun geben-dbgp-redirect-init (session)
  "Initialize redirection related variables."
  (let ((stdout (geben-redirect-stdout (geben-session-redirect session)))
        (stderr (geben-redirect-stderr (geben-session-redirect session))))
    (when stdout
      (geben-dbgp-command-stdout session stdout))
    (when stderr
      (geben-dbgp-command-stderr session stderr))))

(defun geben-dbgp-handle-stream (session msg)
  "Handle a stream message."
  (let ((type (cl-case (intern-soft (xml-get-attribute msg 'type))
                ('stdout :stdout)
                ('stderr :stderr)))
        (encoding (xml-get-attribute msg 'encoding))
        (content (car (last msg))))
    (geben-dbgp-redirect-stream session type encoding content)))

(defun geben-dbgp-redirect-stream (session type encoding content)
  "Print redirected string to specific buffers."
  (let ((buf (geben-session-redirect-buffer session type))
        save-pos)
    (when buf
      (with-current-buffer buf
        (setq save-pos (unless (eobp) (point)))
        (save-excursion
          (goto-char (point-max))
          (insert (decode-coding-string
                   (if (string= "base64" encoding)
                       (base64-decode-string content)
                     content)
                   (geben-redirect-coding-system (geben-session-redirect session)))))
        (goto-char (or save-pos
                       (point-max))))
      (when geben-show-redirect-buffers
        (geben-dbgp-display-window buf)))))

(defun geben-dbgp-command-stdout (session mode)
  "Send `stdout' command."
  (let ((m (plist-get '(nil 0 :disable 0 :redirect 1 :intercept 2) mode)))
    (when (and m)
      (geben-dbgp-send-command session "stdout" (cons "-c" m)))))

(defun geben-dbgp-response-stdout (session cmd msg)
  "A response message handler for `stdout' command."
  (setf (geben-redirect-stdout (geben-session-redirect session))
        (cl-case (geben-cmd-param-get cmd "-c")
          (0 nil)
          (1 :redirect)
          (2 :intercept))))

(defun geben-dbgp-command-stderr (session mode)
  "Send `stderr' command."
  (let ((m (plist-get '(nil 0 :disable 0 :redirect 1 :intercept 2) mode)))
    (when (and m)
      (geben-dbgp-send-command session "stderr" (cons "-c" m)))))

(defun geben-dbgp-response-stderr (session cmd msg)
  "A response message handler for `stderr' command."
  (setf (geben-redirect-stderr (geben-session-redirect session))
        (cl-case (geben-cmd-param-get cmd "-c")
          (0 nil)
          (1 :redirect)
          (2 :intercept))))


;;==============================================================
;; DBGp starter
;;==============================================================

(defun geben-dbgp-start (host port)
  "Create DBGp listeners at each CONNECTION-POINTS."
  (condition-case error-sexp
      (let* ((result (dbgp-exec host port
                                :session-accept 'geben-dbgp-session-accept-p
                                :session-init 'geben-dbgp-session-init
                                :session-filter 'geben-dbgp-session-filter
                                :session-sentinel 'geben-dbgp-session-sentinel))
             (listener (and (consp result)
                            (car result))))
        (when (processp listener)
          (message "Waiting for debug server to connect at port %s." port)))
    (error
     (beep)
     (read-char (format "[port %s] %s" port (cl-second error-sexp))
                nil 3))))

(defun geben-dbgp-start-proxy (ip-or-addr port idekey ;;multi-session-p
                                          session-port)
  "Create DBGp listeners at each CONNECTION-POINTS."
  ;; FIXME: Add host possibility
  (condition-case error-sexp
      (let* ((result
              (dbgp-proxy-register-exec ip-or-addr port idekey nil ;; multi-session-p
                                        session-port
                                        :session-accept 'geben-dbgp-session-accept-p
                                        :session-init 'geben-dbgp-session-init
                                        :session-filter 'geben-dbgp-session-filter
                                        :session-sentinel 'geben-dbgp-session-sentinel))
             (listener (and (consp result)
                            (car result))))
        (when (processp listener)
          (message "Waiting for debug server to connect.")))
    (error
     (beep)
     (read-char (format "[proxy %s:%s-%s] %s"
                        ip-or-addr port idekey (cl-second error-sexp))
                nil 3))))

(defun geben-dbgp-session-accept-p (proc)
  "Judge whether the SESSION is to be processed or to be terminated."
  ;; accept the new session if:
  ;;  a. capable for multi sessions.
  ;;  b. not used yet; it's the first session for the connection-point.
  (let ((accept-p
         ;;string is set init dbgp-comint-setup
         (if (dbgp-proxy-p proc)
             (let ((proxy (dbgp-plist-get proc :proxy)))
               (or (plist-get proxy :multi-session)
                   (not (cl-some (lambda (session)
                                   (eq proxy (dbgp-plist-get proc :proxy)))
                                 geben-sessions))))
           (let ((port (dbgp-port-get (dbgp-listener-get proc))))
             (not (cl-some (lambda (session)
                             (let ((oproc (geben-session-process session)))
                               (and oproc
                                    (not (dbgp-proxy-p oproc))
                                    (eq port (dbgp-port-get (dbgp-listener-get oproc))))))
                           geben-sessions)))))
        (user-filtered-reason (geben-dbgp-session-user-filter proc string)))
    (unless accept-p
      (message "GEBEN: Rejected new connection from %s (Already in debugging)"
               (car (process-contact proc))))
    (when user-filtered-reason
      (message "GEBEN: Rejected new connection from %s due to %s"
               (car (process-contact proc)) user-filtered-reason))
    (and accept-p (not user-filtered-reason))))

(defcustom geben-dbgp-session-user-filter-uri-regexp nil
  "Ignore all debug requests with matched uris"
  :group 'geben
  :type '(repeat string))

(defcustom geben-dbgp-session-user-filter-uri-regexp-hitcounts '()
  "Plist of matched uris and the required amount of hits to trigger."
  :type '(plist :value-type integer :key-type string)
  :group 'geben)

(defvar geben-dbgp-session-user-filter-uri-regexp-hitcounts-hit '())

(defcustom geben-dbgp-session-user-filter-uri-regexp nil
  "Ignore all debug requests with matched uris"
  :group 'geben
  :type '(repeat string))

(defcustom geben-dbgp-session-user-filter-interactively-ask nil
  "Enable to get asked for each URI to start a geben session for it."
  :group 'geben
  :type 'boolean)

(defun geben-dbgp-session-user-filter (proc string)
  "Do not accept a session if its fileuri can be matched against "
  (let* ((xml (car (with-temp-buffer
                     (insert string)
                     (xml-parse-region (point-min) (point-max)))))
         (fileuri (xml-get-attribute-or-nil xml 'fileuri)))
    (when (or
           (and geben-dbgp-session-user-filter-interactively-ask
                (not (yes-or-no-p (concat "geben accept: " fileuri))))
           ;; could probably be done in a nicer way
           (cl-some 'identity
                    (cl-loop for (reg value) on geben-dbgp-session-user-filter-uri-regexp-hitcounts by 'cddr
                             if (string-match reg fileuri)
                             collect (let ((hitcount (1+ (or (lax-plist-get geben-dbgp-session-user-filter-uri-regexp-hitcounts-hit reg) 0))))
                                       (setq geben-dbgp-session-user-filter-uri-regexp-hitcounts-hit
                                             (lax-plist-put geben-dbgp-session-user-filter-uri-regexp-hitcounts-hit reg hitcount))
                                       ;; reject if less hits
                                       (< hitcount value))))
           (cl-some (lambda (reg)
                      (string-match reg fileuri)) geben-dbgp-session-user-filter-uri-regexp))
      fileuri)))

;; cleanup hits after session
(defun geben-dbgp-session-user-filter-cleanup (session)
  (setq geben-dbgp-session-user-filter-uri-regexp-hitcounts-hit '()))
(add-hook 'geben-session-exit-hook 'geben-dbgp-session-user-filter-cleanup)

(defun geben-dbgp-session-init (proc)
  "Initialize SESSION environment."
  (let ((session (geben-session-make :process proc)))
    (push session geben-sessions)
    (dbgp-plist-put proc :session session)
    (with-current-buffer (process-buffer proc)
      (set (make-local-variable 'geben-current-session) session)
      (rename-buffer (geben-session-buffer-name session geben-process-buffer-name) t))))

(defun geben-dbgp-session-filter (proc string)
  "Process DBGp response STRING.
Parse STRING, find xml chunks, convert them to xmlized lisp objects
and call `geben-dbgp-entry' with each chunk."
  (let ((session (dbgp-plist-get proc :session))
        xml output)
    (with-temp-buffer
      (insert string)
      (setq output
            (or (ignore-errors
                  (setq xml (xml-parse-region (point-min) (point-max)))
                  (goto-char (point-min))
                  (when (re-search-forward "\\?>" nil t)
                    (delete-region (match-end 0) (point-max))
                    (insert "\n")
                    (xml-print xml)
                    (propertize (buffer-string)
                                'front-sticky t
                                'font-lock-face 'dbgp-response-face)))
                string)))
    (when xml
      (condition-case error-sexp
          (geben-dbgp-entry session (car xml))
        (error
         (warn "GEBEN internal error: %S" error-sexp))))
    output))

(defun geben-dbgp-session-sentinel (proc string)
  (when (buffer-live-p (process-buffer proc))
    (dbgp-session-echo-input proc "\nDisconnected.\n\n"))
  (let ((session (dbgp-plist-get proc :session)))
    (when session
      (ignore-errors
        (geben-session-release session))
      (accept-process-output)
      (setq geben-sessions (remq session geben-sessions)))))

(add-hook 'kill-emacs-hook (lambda ()
                             (dolist (session geben-sessions)
                               (ignore-errors
                                 (geben-session-release session)))))


;;==============================================================
;; DBGp connected session initialization
;;==============================================================

(defun geben-dbgp-init-fetch-entry-source (session)
  "Fetch the content of the entry source file."
  (let ((fileuri (xml-get-attribute-or-nil (geben-session-initmsg session) 'fileuri)))
    (when fileuri
      (geben-dbgp-command-source session fileuri))))

(defun geben-dbgp-first-continuous-command (session)
  ""
  (geben-dbgp-sequence
      (geben-dbgp-send-command session "status")
    (lambda (session cmd msg err)
      (unless err
        (if (not geben-pause-at-entry-line)
            (geben-dbgp-command-run session)
          (if (and (equal "break" (xml-get-attribute msg 'status))
                   (not (member (geben-session-language session) '(:perl))))
              ;; it is nonconforming to DBGp specs; anyway manage it.
              (run-hook-with-args 'geben-dbgp-continuous-command-hook session)
            (geben-dbgp-command-step-into session)))))))

;; features

(defcustom geben-dbgp-feature-list
  '((:set max_data 32768)
    (:set max_depth 1)
    (:set max_children 32)
    (:get breakpoint_types geben-dbgp-breakpoint-store-types))
  "*Specifies set of feature variables for each new debugging session.
Each entry forms a list (METHOD FEATURE_NAME VALUE_OR_CALLBACK).
METHOD is either `:get' or `:set'.
FEATURE_NAME is a feature name described in DBGp specification.
VALUE_OR_CALLBACK is, if the METHOD is `:get' then it should
be symbol of a callback function will be invoked 3 arguments
\(CMD MSG ERR), which are results of feature_get DBGp command.
If the method is `:set' VALUE_OR_CALLBACK can be either a value
or a symbol of a function. In the latter case the result value
of the function is passed to feature_set DBGp command."
  :group 'geben
  :type '(repeat (list (radio (const :get)
                              (const :set))
                       (radio (const :help-echo ":get" :tag "language_supports_threads (:get)" language_supports_threads)
                              (const :tag "language_name (:get)" language_name)
                              (const :tag "encoding (:get)" encoding)
                              (const :tag "protocol_version (:get)" protocol_version)
                              (const :tag "supports_async (:get)" supports_async)
                              (const :tag "data_encoding (:get)" data_encoding)
                              (const :tag "breakpoint_languages (:get)" breakpoint_languages)
                              (const :tag "breakpoint_types (:get)" breakpoint_types)
                              (const :tag "multiple_sessions (:get :set)" multiple_sessions)
                              (const :tag "encoding (:get :set)" encoding)
                              (const :tag "max_children (:get :set)" max_children)
                              (const :tag "max_data (:get :set)" max_data)
                              (const :tag "max_depth (:get :set)" max_depth)
                              (const :tag "supports_postmortem (:get)" supports_postmortem)
                              (const :tag "show_hidden (:get :set)" show_hidden)
                              (const :tag "notify_ok (:get :set)" notify_ok))
                       sexp)))

(defun geben-dbgp-feature-init (session)
  "Configure debugger engine with value of `geben-dbgp-feature-list'."
  (let ((features (or (geben-session-feature session)
                      geben-dbgp-feature-list)))
    (dolist (entry features)
      (let ((method (car entry))
            (name (symbol-name (nth 1 entry)))
            (param (nth 2 entry)))
        (cl-case method
          (:set
           (let ((value (cond
                         ((null param) nil)
                         ((symbolp param)
                          (if (fboundp param)
                              (funcall param)
                            (if (boundp param)
                                (symbol-value param)
                              (symbol-name param))))
                         (t param))))
             (geben-dbgp-command-feature-set session name value)))
          (:get
           (condition-case error-sexp
               (if (and (symbolp param)
                        (fboundp param))
                   (geben-dbgp-sequence
                       (geben-dbgp-command-feature-get session name)
                     param))
             (error
              (warn "`geben-dbgp-feature-alist' has invalid entry: %S" entry)))))))))

;; feature

(defun geben-dbgp-command-feature-get (session feature)
  "Send \`feature_get\' command."
  (geben-dbgp-send-command session "feature_get" (cons "-n" feature)))

(defun geben-dbgp-command-feature-set (session feature value)
  "Send \`feature_get\' command."
  (geben-dbgp-send-command session "feature_set"
                           (cons "-n" feature)
                           (cons "-v" (format "%S" (eval value)))))

                                        ;(add-hook 'geben-dbgp-init-hook #'geben-dbgp-init-fetch-entry-source t)
(add-hook 'geben-dbgp-init-hook #'geben-dbgp-feature-init t)
(add-hook 'geben-dbgp-init-hook #'geben-dbgp-redirect-init t)
(add-hook 'geben-dbgp-init-hook #'geben-dbgp-command-context-names t)
(add-hook 'geben-dbgp-init-hook #'geben-dbgp-breakpoint-restore t)
(add-hook 'geben-dbgp-init-hook #'geben-dbgp-first-continuous-command t)

(add-hook 'geben-dbgp-continuous-command-hook #'geben-dbgp-stack-update)
(add-hook 'geben-dbgp-continuous-command-hook #'geben-dbgp-breakpoint-list-refresh)
(add-hook 'geben-dbgp-stack-update-hook #'geben-context-list-refresh)


;;==============================================================
;;  geben-mode
;;==============================================================

(defcustom geben-query-on-clear-breakpoints t
  "*Specify if query is needed before removing all breakpoints.
If non-nil, GEBEN will query the user before removing all breakpoints."
  :group 'geben
  :type 'boolean)

(defcustom geben-scroll-margin nil
  "Controls the scroll margin of the geben mode."
  :group 'geben
  :type '(choice integer (const nil)))

(defvar geben-mode-map nil)
(unless geben-mode-map
  (setq geben-mode-map (make-sparse-keymap "geben"))
  ;; control
  (define-key geben-mode-map " " 'geben-step-again)
  (define-key geben-mode-map "g" 'geben-run)
  ;;(define-key geben-mode-map "G" 'geben-Go-nonstop-mode)
  (define-key geben-mode-map ">" 'geben-set-redirect)
  ;;(define-key geben-mode-map "T" 'geben-Trace-fast-mode)
  (define-key geben-mode-map "c" 'geben-run-to-cursor)
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
  (define-key geben-mode-map "v" 'geben-display-context)

  ;; breakpoints
  (define-key geben-mode-map "b" 'geben-set-breakpoint-line)
  (define-key geben-mode-map "B" 'geben-breakpoint-menu)
  (define-key geben-mode-map "u" 'geben-unset-breakpoint-line)
  (define-key geben-mode-map "U" 'geben-clear-breakpoints)
  (define-key geben-mode-map "\C-cb" 'geben-show-breakpoint-list)
  ;;(define-key geben-mode-map "B" 'geben-next-breakpoint)
  ;;(define-key geben-mode-map "x" 'geben-set-conditional-breakpoint)
  ;;(define-key geben-mode-map "X" 'geben-set-global-break-condition)

  ;; evaluation
  (define-key geben-mode-map "e" 'geben-eval-expression)
  (define-key geben-mode-map "l" 'geben-eval-current-line)
  ;;(define-key geben-mode-map "E" 'geben-eval-current-word)
  ;;(define-key geben-mode-map "\C-x\C-e" 'geben-eval-last-sexp)

  ;; views
  (define-key geben-mode-map "w" 'geben-where)
  ;;(define-key geben-mode-map "v" 'geben-view-outside) ;; maybe obsolete??
  ;;(define-key geben-mode-map "p" 'geben-bounce-point)
  ;;(define-key geben-mode-map "P" 'geben-view-outside) ;; same as v
  ;;(define-key geben-mode-map "W" 'geben-toggle-save-windows)

  ;; misc
  (define-key geben-mode-map "?" 'geben-mode-help)
  (define-key geben-mode-map "d" 'geben-show-backtrace)
  (define-key geben-mode-map "t" 'geben-show-backtrace)
  (define-key geben-mode-map "\C-cp" 'geben-toggle-pause-at-entry-line-flag)
  (define-key geben-mode-map "\C-cf" 'geben-find-file)

  ;;(define-key geben-mode-map "-" 'negative-argument)

  ;; statistics
  ;;(define-key geben-mode-map "=" 'geben-temp-display-freq-count)

  ;; GUD bindings
  (define-key geben-mode-map "\C-c\C-s" 'geben-step-into)
  (define-key geben-mode-map "\C-c\C-n" 'geben-step-over)
  (define-key geben-mode-map "\C-c\C-c" 'geben-run)

  (define-key geben-mode-map "\C-x " 'geben-set-breakpoint-line)
  (define-key geben-mode-map "\C-c\C-d" 'geben-unset-breakpoint-line)
  (define-key geben-mode-map "\C-c\C-t" 'geben-set-breakpoint-line)
  (define-key geben-mode-map "\C-c\C-l" 'geben-where))

;;;###autoload
(define-minor-mode geben-mode
  "Minor mode for debugging source code with GEBEN.
The geben-mode buffer commands:
\\{geben-mode-map}"
  nil " *debugging*" geben-mode-map
  (setq buffer-read-only geben-mode)
  (setq left-margin-width (if geben-mode 2 0))
  ;; when the buffer is visible in a window,
  ;; force the window to notice the margin modification
  (set (make-local-variable 'command-error-function) #'geben-mode-read-only-handler)
  (let ((win (get-buffer-window (current-buffer))))
    (if win
        (set-window-buffer win (current-buffer))))
  (when geben-scroll-margin
    (setq-local scroll-margin geben-scroll-margin)))

(add-hook 'geben-source-visit-hook 'geben-enter-geben-mode)
(add-hook 'geben-session-enter-hook 'geben-set-predefined-breakpoints t)

(defun geben-mode-read-only-handler (data context caller)
  (if (eq 'buffer-read-only (car data))
      (geben-with-current-session session
        (let ((prompt "The buffer is under debug mode. Want to open the original file? (y/N): "))
          (if (memq (read-char prompt) '(?Y ?y))
              (geben-session-source-visit-original-file
               session
               (geben-session-source-fileuri session (buffer-file-name))))))
    (message (error-message-string data))
    (beep)))

(defun geben-enter-geben-mode (session buf)
  (with-current-buffer buf
    (geben-mode 1)
    (set (make-local-variable 'geben-current-session) session)))

(add-hook 'geben-source-release-hook
          (lambda () (geben-mode 0)))

(defun geben-where ()
  "Move to the current breaking point."
  (interactive)
  (geben-with-current-session session
    (if (geben-session-stack session)
        (let* ((stack (cl-second (car (geben-session-stack session))))
               (fileuri (geben-source-fileuri-regularize (cdr (assq 'filename stack))))
               (lineno (cdr (assq 'lineno stack))))
          (geben-session-cursor-update session fileuri lineno))
      (when (interactive-p)
        (message "GEBEN is not started.")))))

(defun geben-quit-window ()
  (interactive)
  (quit-window)
  (geben-where))

(defun geben-mode-help ()
  "Display description and key bindings of `geben-mode'."
  (interactive)
  (describe-function 'geben-mode))

(defvar geben-step-type :step-into
  "Step command of what `geben-step-again' acts.
This value remains the last step command type either
`:step-into' or `:step-out'.")

(defun geben-step-again ()
  "Do either `geben-step-into' or `geben-step-over' what the last time called.
Default is `geben-step-into'."
  (interactive)
  (cl-case geben-step-type
    (:step-over (geben-step-over))
    (:step-into (geben-step-into))
    (t (geben-step-into))))

(defun geben-step-into ()
  "Step into the definition of the function or method about to be called.
If there is a function call involved it will break on the first
statement in that function"
  (interactive)
  (setq geben-step-type :step-into)
  (geben-with-current-session session
    (geben-dbgp-command-step-into session)))

(defun geben-step-over ()
  "Step over the definition of the function or method about to be called.
If there is a function call on the line from which the command
is issued then the debugger engine will stop at the statement
after the function call in the same scope as from where the
command was issued"
  (interactive)
  (setq geben-step-type :step-over)
  (geben-with-current-session session
    (geben-dbgp-command-step-over session)))

(defun geben-step-out ()
  "Step out of the current scope.
It breaks on the statement after returning from the current
function."
  (interactive)
  (geben-with-current-session session
    (geben-dbgp-command-step-out session)))

(defun geben-run ()
  "Start or resumes the script.
It will break at next breakpoint, or stops at the end of the script."
  (interactive)
  (geben-with-current-session session
    (geben-dbgp-command-run session)))

(defun geben-run-to-cursor ()
  "Run the script to where the cursor points."
  (interactive)
  (geben-with-current-session session
    (geben-dbgp-sequence
        (geben-set-breakpoint-line nil nil nil t)
      (lambda (session cmd msg err)
        (let ((bid (xml-get-attribute-or-nil msg 'id)))
          (geben-dbgp-sequence-bind (bid)
            (geben-run)
            (lambda (session cmd msg err)
              (geben-dbgp-command-breakpoint-remove session bid))))))))

(defun geben-stop ()
  "End execution of the script immediately."
  (interactive)
  (geben-with-current-session session
    (geben-dbgp-command-stop session)))

(defun geben-breakpoint-menu (arg)
  "Set a breakpoint interactively.
Script debugger engine may support a kind of breakpoints, which
will be stored in the variable `geben-dbgp-breakpoint-types'
after a debugging session is started.

This command asks you a breakpoint type and its options.
Optionally, with a numeric argument you can specify `hit-value'
\(number of hits to break); \\[universal-argument] 2 \
\\<geben-mode-map>\\[geben-breakpoint-menu] will set a breakpoint
with 2 hit-value.
With just a prefix arg \(\\[universal-argument] \\[geben-breakpoint-menu]), \
this command will also ask a
hit-value interactively.
"
  (interactive "P")
  (geben-with-current-session session
    (let ((candidates (remove nil
                              (mapcar
                               (lambda (x)
                                 (if (member (car x)
                                             (geben-breakpoint-types (geben-session-breakpoint session)))
                                     x))
                               '((:line . "l)Line")
                                 (:call . "c)Call")
                                 (:return . "r)Return")
                                 (:exception . "e)Exception")
                                 (:conditional . "d)Conditional")
                                 (:watch . "w)Watch"))))))
      (when (null candidates)
        (error "No breakpoint type is supported by the debugger engine."))
      (let* ((c (read-char (concat "Breakpoint type: "
                                   (mapconcat
                                    (lambda (x)
                                      (cdr x))
                                    candidates " "))))
             (x (cl-find-if (lambda (x)
                              (eq c (elt (cdr x) 0)))
                            candidates))
             (fn (and x
                      (intern-soft (concat "geben-set-breakpoint-"
                                           (substring (symbol-name (car x)) 1))))))
        (unless x
          (error "Cancelled"))
        (if (fboundp fn)
            (call-interactively fn)
          (error (concat (symbol-name fn) " is not implemented.")))))))

(defun geben-set-breakpoint-common (session hit-value bp)
  (setq hit-value (if (and (not (null hit-value))
                           (listp hit-value))
                      (if (fboundp 'read-number)
                          (read-number "Number of hit to break: ")
                        (string-to-number
                         (read-string "Number of hit to break: ")))
                    hit-value))
  (plist-put bp :hit-value (if (and (numberp hit-value)
                                    (<= 0 hit-value))
                               hit-value
                             0))
  (geben-dbgp-command-breakpoint-set session bp))

(defun geben-set-breakpoint-line (fileuri lineno &optional hit-value temporary-p)
  "Set a breakpoint at the current line.
Optionally, with a numeric argument you can specify `hit-value'
\(number of hits to break); \\[universal-argument] 2 \
\\<geben-mode-map>\\[geben-set-breakpoint-line] will set a breakpoint
with 2 hit-value.
With just a prefix arg \(\\[universal-argument] \\[geben-set-breakpoint-line]), \
this command will also ask a
hit-value interactively."
  (interactive (list nil nil current-prefix-arg nil))
  (geben-with-current-session session
    (let ((local-path (if fileuri
                          (geben-session-source-local-path session fileuri)
                        (buffer-file-name (current-buffer)))))
      (geben-set-breakpoint-common session hit-value
                                   (geben-bp-make
                                    session :line
                                    :fileuri (or fileuri
                                                 (geben-session-source-fileuri session local-path)
                                                 (geben-session-source-fileuri session (file-truename local-path))
                                                 (geben-source-fileuri session local-path))
                                    :lineno (if (numberp lineno)
                                                lineno
                                              (geben-what-line))
                                    :local-path local-path
                                    :overlay t
                                    :run-once temporary-p)))))

(defvar geben-set-breakpoint-call-history nil)
(defvar geben-set-breakpoint-fileuri-history nil)
(defvar geben-set-breakpoint-exception-history nil)
(defvar geben-set-breakpoint-condition-history nil)

(defun geben-set-breakpoint-call (name &optional fileuri hit-value)
  "Set a breakpoint to break at when entering function/method named NAME.
For a class method, specify NAME like \"MyClass::MyMethod\".
For an instance method, do either like \"MyClass::MyMethod\" or
\"MyClass->MyMethod\".
Optionally, with a numeric argument you can specify `hit-value'
\(number of hits to break); \\[universal-argument] 2 \
\\<geben-mode-map>\\[geben-set-breakpoint-call] will set a breakpoint
with 2 hit-value.
With just a prefix arg \(\\[universal-argument] \\[geben-set-breakpoint-call]),
this command will also ask a
hit-value interactively."
  (interactive (list nil))
  (geben-with-current-session session
    (when (interactive-p)
      (setq name (read-string "Name: " ""
                              'geben-set-breakpoint-call-history))
      (setq fileuri
            (unless (member (geben-session-language session) '(:php :ruby))
              ;; at this present some debugger engines' implementations is buggy:
              ;; some requires fileuri and some don't accept it.
              (let ((local-path (file-truename (buffer-file-name (current-buffer)))))
                (read-string "fileuri: "
                             (or (geben-session-source-fileuri session local-path)
                                 (geben-source-fileuri session local-path))
                             'geben-set-breakpoint-fileuri-history))))
      (setq hit-value current-prefix-arg))
    (when (string< "" name)
      (geben-set-breakpoint-common session hit-value
                                   (geben-bp-make session :call
                                                  :function name
                                                  :fileuri fileuri)))))

(defun geben-set-breakpoint-return (name &optional fileuri hit-value)
  "Set a breakpoint to break after returned from a function/method named NAME.
For a class method, specify NAME like \"MyClass::MyMethod\".
For an instance method, do either like \"MyClass::MyMethod\" or
\"MyClass->MyMethod\".
Optionally, with a numeric argument you can specify `hit-value'
\(number of hits to break); \\[universal-argument] 2 \
\\<geben-mode-map>\\[geben-set-breakpoint-return] will set a breakpoint
with 2 hit-value.
With just a prefix arg \(\\[universal-argument] \\[geben-set-breakpoint-return]),
this command will also ask a
hit-value interactively."
  (interactive (list nil))
  (geben-with-current-session session
    (when (interactive-p)
      (setq name (read-string "Name: " ""
                              'geben-set-breakpoint-call-history))
      (setq fileuri
            (unless (member (geben-session-language session) '(:php :ruby))
              ;; at this present some debugger engines' implementations are buggy:
              ;; some requires fileuri and some don't accept it.
              (let ((local-path (file-truename (buffer-file-name (current-buffer)))))
                (read-string "fileuri: "
                             (or (geben-session-source-fileuri session local-path)
                                 (geben-source-fileuri session local-path))
                             'geben-set-breakpoint-fileuri-history))))
      (setq hit-value current-prefix-arg))
    (when (string< "" name)
      (geben-set-breakpoint-common session hit-value
                                   (geben-bp-make session :return
                                                  :function name
                                                  :fileuri fileuri)))))

(defun geben-set-breakpoint-exception (name &optional hit-value)
  "Set a breakpoint to break at when an exception named NAME is occurred.
Optionally, with a numeric argument you can specify `hit-value'
\(number of hits to break); \\[universal-argument] 2 \
\\<geben-mode-map>\\[geben-set-breakpoint-exception] will set a breakpoint
with 2 hit-value.
With just a prefix arg \(\\[universal-argument] \\[geben-set-breakpoint-exception]),
this command will also ask a
hit-value interactively."
  (interactive (list
                (read-string "Exception type: "
                             "Exception"
                             'geben-set-breakpoint-exception-history)
                current-prefix-arg))
  (geben-with-current-session session
    (geben-set-breakpoint-common session hit-value
                                 (geben-bp-make session :exception
                                                :exception name))))

(defun geben-set-breakpoint-conditional (expr fileuri &optional lineno hit-value)
  "Set a breakpoint to break at when the expression EXPR is true in the file FILEURI.
Optionally, with a numeric argument you can specify `hit-value'
\(number of hits to break); \\[universal-argument] 2 \
\\<geben-mode-map>\\[geben-set-breakpoint-conditional] will set a breakpoint
with 2 hit-value.
With just a prefix arg \(\\[universal-argument] \\[geben-set-breakpoint-conditional]),
this command will also ask a
hit-value interactively."
  (interactive (list nil nil))
  (geben-with-current-session session
    (when (interactive-p)
      (setq expr (read-string "Expression: " ""
                              'geben-set-breakpoint-condition-history))
      (setq fileuri
            (let ((local-path (file-truename (buffer-file-name (current-buffer)))))
              (or (geben-session-source-fileuri session local-path)
                  (geben-source-fileuri session local-path))))
      (setq lineno (read-string "Line number to evaluate (blank means entire file): "
                                (number-to-string (geben-what-line))))
      (setq hit-value current-prefix-arg))

    (geben-set-breakpoint-common session hit-value
                                 (geben-bp-make session :conditional
                                                :expression expr
                                                :fileuri fileuri
                                                :lineno (and (stringp lineno)
                                                             (string-match "^[0-9]+$" lineno)
                                                             (string-to-number lineno))))))

(defun geben-set-breakpoint-watch (expr &optional hit-value)
  "Set a breakpoint to break on write of the variable or address.
Optionally, with a numeric argument you can specify `hit-value'
\(number of hits to break); \\[universal-argument] 2 \
\\<geben-mode-map>\\[geben-set-breakpoint-conditional] will set a breakpoint
with 2 hit-value.
With just a prefix arg \(\\[universal-argument] \\[geben-set-breakpoint-conditional]),
this command will also ask a
hit-value interactively."
  (interactive (list nil))
  (geben-with-current-session session
    (when (interactive-p)
      (setq expr (read-string "Expression: " ""
                              'geben-set-breakpoint-condition-history))
      (setq hit-value current-prefix-arg))
    (geben-set-breakpoint-common session hit-value
                                 (geben-bp-make session :watch
                                                :expression expr))))

(defun geben-unset-breakpoint-line ()
  "Clear a breakpoint set at the current line."
  (interactive)
  (geben-with-current-session session
    (mapc (lambda (bp)
            (geben-dbgp-command-breakpoint-remove session (plist-get bp :id)))
          (geben-breakpoint-find-at-pos session (current-buffer) (point)))))

(defun geben-clear-breakpoints ()
  "Clear all breakpoints.
If `geben-query-on-clear-breakpoints' is non-nil, GEBEN will query the user before
removing all breakpoints."
  (interactive)
  (geben-with-current-session session
    (when (or (not geben-query-on-clear-breakpoints)
              (let ((prompt "Clear all breakpoints? (y/N): "))
                (memq (read-char prompt) '(?Y ?y))))
      (geben-breakpoint-clear session)
      (geben-clear-predefined-breakpoints))))

(defun geben-show-breakpoint-list ()
  "Display breakpoint list.
The breakpoint list buffer is under `geben-breakpoint-list-mode'.
Key mapping and other information is described its help page."
  (interactive)
  (geben-breakpoint-list-refresh t))

(defcustom geben-predefined-breakpoints nil
  "Controls the association list of predefined breakpoints.
The key of each association specifies the file name of the
breakpoint and the value speficies the line number."
  :group 'geben
  :type '(alist :key-type string :value-type integer))

(defun geben-set-predefined-breakpoints (session)
  "Set the predefined breakpoints as SESSION breakpoints."
  (when geben-predefined-breakpoints
    (dolist (bp geben-predefined-breakpoints)
      (let ((local-path (car bp))
            (line-number (cdr bp)))
        (geben-set-breakpoint-common session nil
                                     (geben-bp-make
                                      session :line
                                      :fileuri (or (geben-session-source-fileuri session local-path)
                                                   (geben-session-source-fileuri session (file-truename local-path))
                                                   (geben-source-fileuri session local-path))
                                      :lineno line-number
                                      :local-path local-path
                                      :overlay t
                                      :run-once nil))))))


(defcustom geben-path-mappings '()
  "Path mappings for setting breakpoints in VMs/containers/other spaceships."
  :group 'geben
  :type '(repeat (list string string)))

(defun geben-path-mappings-to-debug (name)
  "Map local developer path to debug path by using replacements from
`geben-path-mappings', replace in order first/car substring by second/cadr ."
  (cl-reduce (lambda (result mapping)
               (replace-regexp-in-string (car mapping) (cadr mapping) result))
             geben-path-mappings
             :initial-value name))

(defun geben-path-mappings-from-debug (name)
  "Map debug path to local developer path by using replacements from
`geben-path-mappings', replace in order second/cadr substring by first/car."
  (cl-reduce (lambda (result mapping)
               (replace-regexp-in-string (cadr mapping) (car mapping) result))
             geben-path-mappings
             :initial-value name))

;;;###autoload
(defun geben-add-current-line-to-predefined-breakpoints ()
  "Add the current line to the predefined breakpoints."
  (interactive)
  (let ((path (geben-path-mappings-to-debug (buffer-file-name)))
        (line (line-number-at-pos)))
    (add-to-list 'geben-predefined-breakpoints `(,path . ,line))
    (message "%s line %s %s" path line "added to predefined breakpoints")))

(defun geben-clear-predefined-breakpoints ()
  "Clear all predefined breakpoints."
  (interactive)
  (setq geben-predefined-breakpoints nil))

(defvar geben-eval-history nil)

(defvar geben-minibuffer-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)
    (define-key map (kbd "TAB") 'dabbrev-expand)
    map)
  "Minibuffer keymap used for reading expressions.")

(defun geben-eval-expression (expr)
  "Evaluate a given string EXPR within the current execution context."
  (interactive
   (progn
     (list (read-from-minibuffer "Eval: "
                                 nil geben-minibuffer-map nil 'geben-eval-history))))
  (geben-with-current-session session
    (geben-dbgp-command-eval session expr))
  (run-hooks 'geben-after-eval-expression))

(defun geben-eval-current-word ()
  "Evaluate a word at where the cursor is pointing."
  (interactive)
  (let ((expr (current-word)))
    (when expr
      (geben-with-current-session session
        (geben-dbgp-command-eval session expr)))))

(defun geben-eval-current-line ()
  "Evaluate the line in which the cursor is located during debugging.
The line will be evaluated within the current execution context
and the result will be displayed in the echo area."
  (interactive)
  (let ((expr (buffer-substring-no-properties (line-beginning-position)
                                              (line-end-position))))
    (when expr
      (geben-with-current-session session
        (geben-dbgp-command-eval session expr)))))

(defun geben-open-file (fileuri)
  "Open a debugger server side file specified by FILEURI.
FILEURI forms like as \`file:///path/to/file\'."
  (interactive (list (read-string "Open file: " "file://")))
  (geben-with-current-session session
    (geben-dbgp-command-source session fileuri)))

(defun geben-show-backtrace ()
  "Display backtrace list.
The backtrace list buffer is under `geben-backtrace-mode'.
Key mapping and other information is described its help page."
  (interactive)
  (geben-with-current-session session
    (geben-backtrace session)))

(defun geben-toggle-pause-at-entry-line-flag ()
  "Toggle `geben-pause-at-entry-line'."
  (interactive)
  (setq geben-pause-at-entry-line
        (not geben-pause-at-entry-line))
  (if (interactive-p)
      (message (format "`geben-pause-at-entry-line' is %s" geben-pause-at-entry-line))))

(defun geben-set-redirect (target &optional arg)
  "Set the debuggee script's output redirection mode.
This command enables you to redirect the debuggee script's output to GEBEN.
You can select redirection target from \`stdout', \`stderr' and both of them.
Prefixed with \\[universal-argument], you can also select redirection mode
from \`redirect', \`intercept' and \`disabled'."
  (interactive (list (cl-case (read-char "Redirect: o)STDOUT e)STRERR b)Both")
                       (?o :stdout)
                       (?e :stderr)
                       (?b :both))
                     current-prefix-arg))
  (unless target
    (error "Cancelled"))
  (let ((mode (if arg
                  (cl-case (read-char "Mode: r)Redirect i)Intercept d)Disable")
                    (?r :redirect)
                    (?i :intercept)
                    (?d :disable))
                :redirect)))
    (unless mode
      (error "Cancelled"))
    (geben-with-current-session session
      (when (memq target '(:stdout :both))
        (geben-dbgp-command-stdout session mode))
      (when (memq target '(:stderr :both))
        (geben-dbgp-command-stderr session mode)))))

(defun geben-display-context (&optional depth)
  (interactive (list (cond
                      ((null current-prefix-arg) 0)
                      ((numberp current-prefix-arg)
                       current-prefix-arg)
                      ((listp current-prefix-arg)
                       (if (fboundp 'read-number)
                           (read-number "Depth: " 0)
                         (string-to-number (read-string "Depth: " "0"))))
                      (t nil))))
  (geben-with-current-session session
    (geben-context-list-display session (or depth 0))))

(defun geben-find-file ()
  (interactive)
  (geben-with-current-session session
    (let ((file-path (geben-session-source-read-file-name
                      session
                      (file-name-directory (geben-source-fileuri session
                                                                 (buffer-file-name)))
                      t)))
      (when file-path
        (geben-open-file (geben-source-fileuri session file-path))))))


(defcustom geben-dbgp-default-port 9000
  "Default port number to listen a new DBGp connection."
  :group 'geben
  :type 'integer)

(defcustom geben-dbgp-default-host "0.0.0.0"
  "Default host to listen a new DBGp connection.

 Default is 0.0.0.0 to catch all connections the user might receive. "
  :group 'geben
  :type 'string)

(defcustom geben-dbgp-default-proxy '("127.0.0.1" 9001 "default" nil t)
  "Default setting for a new DBGp proxy connection.

The first and second elements are address and port where the DBGp proxy listening on.
The third element is IDE key.
The forth element is a flag but currently not used yet.
The fifth element is port to be used in debugging sessions. If a non-integer value is
set, then any free port will be allocated.
"
  :group 'geben)

;;;###autoload
(defun geben (&optional args)
  "Start GEBEN, a DBGp protocol frontend - a script debugger.
Variations are described below.

By default, starts GEBEN listening to port `geben-dbgp-default-port'.
Prefixed with one \\[universal-argument], asks listening port number interactively and
starts GEBEN on the port.
Prefixed with two \\[universal-argument]'s, starts a GEBEN proxy listener.
Prefixed with three \\[universal-argument]'s, kills a GEBEN listener.
Prefixed with four \\[universal-argument]'s, kills a GEBEN proxy listener.

GEBEN communicates with script servers, located anywhere local or
remote, in DBGp protocol (e.g. PHP with Xdebug extension)
to help you debugging your script with some valuable features:
 - continuation commands like \`step in\', \`step out\', ...
 - a kind of breakpoints like \`line no\', \`function call\' and
   \`function return\'.
 - evaluation
 - stack dump
 - etc.

The script servers should be DBGp protocol enabled.
Ask to your script server administrator about this setting up
issue.

Once you've done these setup operation correctly, run GEBEN first
and your script on your script server second. After some
negotiation GEBEN will display your script's entry source code.
The debugging session is started.

In the debugging session the source code buffers are under the
minor mode  `geben-mode'. Key mapping and other information is
described its help page."
  (interactive "p")
  (cl-case args
    (1
     (geben-dbgp-start geben-dbgp-default-host geben-dbgp-default-port))
    (4
     (geben-dbgp-start (dbgp-read-host) (dbgp-read-port)))
    (16
     (call-interactively 'geben-proxy))
    (64
     (call-interactively 'geben-end))
    (t
     (call-interactively 'geben-proxy-end))))

(defun geben-end (port)
  "Stop the DBGp listener on PORT."
  (interactive
   (let ((ports (remq nil
                      (mapcar (lambda (listener)
                                (and (not (dbgp-proxy-p listener))
                                     (number-to-string (cl-second (process-contact listener)))))
                              dbgp-listeners))))
     (list
      (if (= 1 (length ports))
          (string-to-number (car ports))
        ;; ask user for the target idekey.
        (let ((num (completing-read "Listener port to kill: " ports nil t)))
          (if (string< "" num)
              (read num)
            (signal 'quit nil)))))))
  (let ((listener (dbgp-listener-find port)))
    (dbgp-listener-kill port)
    (and (interactive-p)
         (message (if listener
                      "The DBGp listener for port %d is terminated."
                    "DBGp listener for port %d does not exist.")
                  port))
    (and listener t)))

(defun geben-proxy (ip-or-addr port idekey ;;multi-session-p
                               &optional session-port)
  "Start a new DBGp proxy listener.
The DBGp proxy should be found at IP-OR-ADDR / PORT.
This create a new DBGp listener and register it to the proxy
associating with the IDEKEY."
  (interactive (list
                (let ((default (or (car dbgp-proxy-address-history)
                                   (nth 0 geben-dbgp-default-proxy)
                                   (nth 0 (default-value 'geben-dbgp-default-proxy)))))
                  (dbgp-read-string (format "Proxy address (default %s): " default)
                                    nil 'dbgp-proxy-address-history default))
                (let ((default (or (car dbgp-proxy-port-history)
                                   (nth 1 geben-dbgp-default-proxy)
                                   (nth 1 (default-value 'geben-dbgp-default-proxy)))))
                  (dbgp-read-integer (format "Proxy port (default %d): " default)
                                     default 'dbgp-proxy-port-history))
                (let ((default (or (car dbgp-proxy-idekey-history)
                                   (nth 2 geben-dbgp-default-proxy)
                                   (nth 2 (default-value 'geben-dbgp-default-proxy)))))
                  (dbgp-read-string "IDE key: " nil 'dbgp-proxy-idekey-history))
                ;;(not (memq (read-char "Multi session(Y/n): ") '(?N ?n)))
                (let ((default (or (car dbgp-proxy-session-port-history)
                                   (nth 4 geben-dbgp-default-proxy)
                                   (nth 4 (default-value 'geben-dbgp-default-proxy)))))
                  (unless (numberp default)
                    (setq default 0))
                  (dbgp-read-integer (format "Port for debug session (%s): "
                                             (if (< 0 default)
                                                 (format "default %d, 0 to use any free port" default)
                                               (format "leave empty to use any free port")))
                                     default 'dbgp-proxy-session-port-history))))
  (geben-dbgp-start-proxy ip-or-addr port idekey ;;multi-session-p
                          session-port))

(defalias 'geben-proxy-end #'dbgp-proxy-unregister)

;; geben full-frame mode

(defvar geben-full-frame-first-buffer nil)

(defun geben-full-frame-save (session)
  (window-configuration-to-register 'geben-full-frame-register)
  (delete-other-windows)
  (setq geben-full-frame-first-buffer t))

(defun geben-full-frame-restore (session)
  (jump-to-register 'geben-full-frame-register t)
  (setq geben-full-frame-first-buffer nil))

;;;###autoload
(define-minor-mode geben-full-frame-mode "" :global t
  (if geben-full-frame-mode
      (progn
        (add-hook 'geben-session-enter-hook 'geben-full-frame-save)
        (add-hook 'geben-session-exit-hook 'geben-full-frame-restore))
    (progn
      (remove-hook 'geben-session-enter-hook 'geben-full-frame-save)
      (remove-hook 'geben-session-exit-hook 'geben-full-frame-restore))))

(provide 'geben)

;;; geben.el ends here
