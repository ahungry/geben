;;; dbgp.el --- DBGp protocol interface

;; Copyright (C) 2005-2010  reedom <fujinaka.tohru@gmail.com>
;; Copyright (C) 2016  Matthew Carter

;; Filename: dbgp.el
;; Author: Matthew Carter <m@ahungry.com>
;; Code derived from Original Author: reedom <fujinaka.tohru@gmail.com>
;; Maintainer: Matthew Carter <m@ahungry.com>
;; URL: https://github.com/ahungry/geben
;; Version: 1.0.0
;; Keywords: DBGp, debugger, PHP, Xdebug, Perl, Python, Ruby, Tcl, Komodo
;; Compatibility: Emacs 24+
;; Package-Requires: ((emacs "24") (cl-lib "0.5"))

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

;;; Code:

(eval-when-compile
  (when (or (not (boundp 'emacs-version))
            (string< emacs-version "24"))
    (error (concat "geben.el: This package requires Emacs 24 or later."))))

(eval-and-compile
  (require 'cl-lib)
  (require 'xml))

(require 'comint)

;;--------------------------------------------------------------
;; customization
;;--------------------------------------------------------------

;; customize group

(defgroup dbgp nil
  "DBGp protocol interface."
  :group 'debug)

(defgroup dbgp-highlighting-faces nil
  "Faces for DBGp process buffer."
  :group 'dbgp
  :group 'font-lock-highlighting-faces)

(defcustom dbgp-default-port 9000
  "DBGp listener's default port number."
  :type 'integer
  :group 'dbgp)

(defcustom dbgp-local-address "127.0.0.1"
  "Local host address.  It is used for DBGp proxy.
This value is passed to DBGp proxy at connection negotiation.
When the proxy receive a new debugging session, the proxy tries
to connect to DBGp listener of this address."
  :type 'string
  :group 'dbgp)

(defface dbgp-response-face
  '((((class color))
     :foreground "brightblue"))
  "Face for displaying DBGp protocol response message."
  :group 'dbgp-highlighting-faces)

(defface dbgp-decoded-string-face
  '((((class color))
     :inherit 'font-lock-string-face))
  "Face for displaying decoded string."
  :group 'dbgp-highlighting-faces)
;;--------------------------------------------------------------
;; utilities
;;--------------------------------------------------------------

(defsubst dbgp-plist-get (proc prop)
  "Return from process PROC the value of property PROP."
  (plist-get (process-plist proc) prop))

(defsubst dbgp-plist-put (proc prop val)
  "In process PROC change PROP to VAL."
  (let ((plist (process-plist proc)))
    (if plist
        (plist-put plist prop val)
      (set-process-plist proc (list prop val)))))

(defsubst dbgp-xml-get-error-node (xml)
  "Return the first node of XML whose child-name is 'error."
  (car
   (xml-get-children xml 'error)))

(defsubst dbgp-xml-get-error-message (xml)
  (let ((err (dbgp-xml-get-error-node xml)))
    (if (stringp (car err))
        (car err)
      (car (xml-node-children
            (car (xml-get-children err 'message)))))))

(defsubst dbgp-make-listner-name (port)
  (format "DBGp listener<%d>" port))

(defsubst dbgp-process-kill (proc)
  "Kill DBGp process PROC."
  (if (memq (process-status proc) '(listen open))
      (delete-process proc))
  ;;  (ignore-errors
  ;;    (with-temp-buffer
  ;;      (set-process-buffer proc (current-buffer)))))
  )

(defsubst dbgp-ip-get (proc)
  (cl-first (process-contact proc)))

(defsubst dbgp-port-get (proc)
  (cl-second (process-contact proc)))

(defsubst dbgp-proxy-p (proc)
  (and (dbgp-plist-get proc :proxy)
       t))

(defsubst dbgp-proxy-get (proc)
  (dbgp-plist-get proc :proxy))

(defsubst dbgp-listener-get (proc)
  (dbgp-plist-get proc :listener))

;;--------------------------------------------------------------
;; DBGp
;;--------------------------------------------------------------

(defcustom dbgp-command-prompt "(cmd) "
  "DBGp client process buffer's command line prompt to display."
  :type 'string
  :group 'dbgp)

;;--------------------------------------------------------------
;; DBGp listener process
;;--------------------------------------------------------------

;; -- What is DBGp listener process --
;;
;; DBGp listener process is a network connection, as an entry point
;; for DBGp protocol connection.
;; The process listens at a specific network address to a specific
;; port for a new session connection(from debugger engine) coming.
;; When a new connection has accepted, the DBGp listener creates
;; a new DBGp session process. Then the new process takes over
;; the connection and the DBGp listener process starts listening
;; for another connection.
;;
;; -- DBGp listener custom properties --
;;
;; :session-init	default function for a new DBGp session
;;			process to initialize a new session.
;; :session-filter	default function for a new DBGp session
;;			process to filter protocol messages.
;; :session-sentinel	default function for a new DBGp session
;;			called when the session is disconnected.

(defvar dbgp-listeners nil
  "List of DBGp listener processes.

DBGp listener process is a network connection, as an entry point
for DBGp protocol connection.
The process listens at a specific network address to a specific
port for a new session connection(from debugger engine) coming.
When a new connection has accepted, the DBGp listener creates
a new DBGp session process.  Then the new process takes over
the connection and the DBGp listener process starts listening
for another connection.

-- DBGp listener process custom properties --

:session-accept		function to determine to accept a new
			DBGp session.
:session-init		function to initialize a new session.
:session-filter		function to filter protocol messages.
:session-sentinel	function called when the session is
			disconnected.
:proxy			if the listener is created for a proxy
			connection, this value has a plist of
			(:addr :port :idekey :multi-session).
			Otherwise the value is nil.")

(defvar dbgp-sessions nil
  "List of DBGp session processes.

DBGp session process is a network connection, talks with a DBGp
debugger engine.

A DBGp session process is created by a DBGp listener process
after a DBGp session connection from a DBGp debugger engine
is accepted.
The session process is alive until the session is disconnected.

-- DBGp session process custom properties --

:listener		The listener process which creates this
			session process.")

(defvar dbgp-listener-port-history nil)
(defvar dbgp-listener-address-history nil)
(defvar dbgp-proxy-address-history nil)
(defvar dbgp-proxy-port-history nil)
(defvar dbgp-proxy-idekey-history nil)
(defvar dbgp-proxy-session-port-history nil)

;;--------------------------------------------------------------
;; interactive read functions
;;--------------------------------------------------------------

(defun dbgp-read-string (prompt &optional initial-input history default-value)
  "Read a string from the terminal, not allowing blanks.
Prompt with PROMPT.  Whitespace terminates the input.
If non-nil, second arg INITIAL-INPUT is a string to insert before reading.
  This argument has been superseded by DEFAULT-VALUE and should normally
  be nil in new code.  It behaves as in `read-from-minibuffer'.  See the
  documentation string of that function for details.
The third arg HISTORY, if non-nil, specifies a history list
  and optionally the initial position in the list.
See `read-from-minibuffer' for details of HISTORY argument.
Fourth arg DEFAULT-VALUE is the default value.  If non-nil, it is used
 for history commands, and as the value to return if the user enters
 the empty string."
  (let (str
        (temp-history (and history
                           (cl-copy-list (symbol-value history)))))
    (while
        (progn
          (setq str (read-string prompt initial-input 'temp-history default-value))
          (if (zerop (length str))
              (setq str (or default-value ""))
            (setq str (replace-regexp-in-string "^[ \t\r\n]+" "" str))
            (setq str (replace-regexp-in-string "[ \t\r\n]+$" "" str)))
          (zerop (length str))))
    (and history
         (set history (cons str (remove str (symbol-value history)))))
    str))

(defun dbgp-read-integer (prompt &optional default history)
  "Read a numeric value in the minibuffer, prompting with PROMPT.
DEFAULT specifies a default value to return if the user just types RET.
The third arg HISTORY, if non-nil, specifies a history list
  and optionally the initial position in the list.
See `read-from-minibuffer' for details of HISTORY argument."
  (let (n
        (temp-history (and history
                           (mapcar 'number-to-string
                                   (symbol-value history)))))
    (while
        (let ((str (read-string prompt nil 'temp-history (if (numberp default)
                                                             (number-to-string default)
                                                           ""))))
          (ignore-errors
            (setq n (cond
                     ((numberp str) str)
                     ((zerop (length str)) default)
                     ((stringp str) (read str)))))
          (unless (integerp n)
            (message "Please enter a number.")
            (sit-for 1)
            t)))
    (and history
         (set history (cons n (remq n (symbol-value history)))))
    n))

(defun dbgp-read-port (&optional preset)
  (interactive)
  (let ((default (or
                  (car dbgp-listener-port-history)
                  preset
                  (default-value 'geben-dbgp-default-port))))
    (dbgp-read-integer (format "Listen port(default %s): " default) default 'dbgp-listener-port-history)))

(defun dbgp-read-host ()
  (interactive)
  (let* ((addrs (append  '("0.0.0.0")  (mapcar (lambda (intf)
                                                 (format-network-address (cdr intf) t))
                                               (network-interface-list))))
         (addr-default (or
                        (car dbgp-listener-address-history)
                        (and (member "127.0.0.1" addrs) "127.0.0.1")
                        (car addrs))))
    (unless addrs
      (error "This machine has no network interface to bind."))
    (completing-read  (format "Listener address to bind: ") addrs nil t nil 'dbgp-listener-address-history)))

;;--------------------------------------------------------------
;; DBGp listener start/stop
;;--------------------------------------------------------------

(defsubst dbgp-listener-find (port)
  (cl-find-if (lambda (listener)
                (eq port (cl-second (process-contact listener))))
              dbgp-listeners))

;;;###autoload
(defun dbgp-start (host port)
  "Start a new DBGp listener listening to PORT."
  (interactive
   (list
    (dbgp-read-host)
    (dbgp-read-port)))
  (let ((result (dbgp-exec host port
                           :session-accept 'dbgp-default-session-accept-p
                           :session-init 'dbgp-default-session-init
                           :session-filter 'dbgp-default-session-filter
                           :session-sentinel 'dbgp-default-session-sentinel)))
    (when (called-interactively-p 'interactive)
      (message (cdr result)))
    result))

;;;###autoload
(defun dbgp-exec (host port &rest session-params)
  "Start a new DBGp listener listening to PORT.
Set the process up with SESSION-PARAMS."
  (if (dbgp-listener-alive-p port)
      (cons (dbgp-listener-find port)
            (format "The DBGp listener for %d has already been started." port))
    (let ((listener (make-network-process :name (dbgp-make-listner-name port)
                                          :host host
                                          :server 1
                                          :service port
                                          :family 'ipv4
                                          :nowait (< emacs-major-version 26) ;; emacs 26 asyncness seems to be too much for now, see issue #4
                                          :noquery t
                                          :filter 'dbgp-comint-setup
                                          :sentinel 'dbgp-listener-sentinel
                                          :log 'dbgp-listener-log)))
      (unless listener
        (error "Failed to create DBGp listener for port %d" port))
      (dbgp-plist-put listener :listener listener)
      (and session-params
           (nconc (process-plist listener) session-params))
      (setq dbgp-listeners (cons listener
                                 (remq (dbgp-listener-find port) dbgp-listeners)))
      (cons listener
            (format "The DBGp listener for %d is started." port)))))

(defun dbgp-stop (port &optional include-proxy)
  "Stop the DBGp listener listening to PORT.
INCLUDE-PROXY may not do anything."
  (interactive
   (let ((ports (remq nil
                      (mapcar (lambda (listener)
                                (and (or current-prefix-arg
                                         (not (dbgp-proxy-p listener)))
                                     (number-to-string (cl-second (process-contact listener)))))
                              dbgp-listeners))))
     (list
      ;; ask user for the target idekey.
      (read (completing-read "Listener port: " ports nil t
                             (and (eq 1 (length ports))
                                  (car ports))))
      current-prefix-arg)))
  (let ((listener (dbgp-listener-find port)))
    (dbgp-listener-kill port)
    (and (called-interactively-p 'interactive)
         (message (if listener
                      "The DBGp listener for port %d is terminated."
                    "DBGp listener for port %d does not exist.")
                  port))
    (and listener t)))

(defun dbgp-listener-kill (port)
  (let ((listener (dbgp-listener-find port)))
    (when listener
      (delete-process listener))))

;;--------------------------------------------------------------
;; DBGp proxy listener register/unregister
;;--------------------------------------------------------------

;;;###autoload
(defun dbgp-proxy-register (proxy-ip-or-addr proxy-port idekey multi-session-p &optional session-port)
  "Register a new DBGp listener to an external DBGp proxy.
The proxy should be found at PROXY-IP-OR-ADDR / PROXY-PORT.
This creates a new DBGp listener and register it to the proxy
associating with the IDEKEY.
MULTI-SESSION-P indicates if multiple sessions are running or not.
SESSION-PORT is either the integer port number, or t."
  (interactive (list
                (let ((default (or (car dbgp-proxy-address-history) "localhost")))
                  (dbgp-read-string (format "Proxy address (default %s): " default)
                                    nil 'dbgp-proxy-address-history default))
                (let ((default (or (car dbgp-proxy-port-history) 9001)))
                  (dbgp-read-integer (format "Proxy port (default %d): " default)
                                     default 'dbgp-proxy-port-history))
                (dbgp-read-string "IDE key: " nil 'dbgp-proxy-idekey-history)
                (not (memq (read-char "Multi session(Y/n): ") '(?N ?n)))
                (let ((default (or (car dbgp-proxy-session-port-history) t)))
                  (unless (numberp default)
                    (setq default 0))
                  (dbgp-read-integer (format "Port for debug session (%s): "
                                             (if (< 0 default)
                                                 (format "default %d, 0 to use any free port" default)
                                               (format "leave empty to use any free port")))
                                     default 'dbgp-proxy-session-port-history))))
  (let ((result (dbgp-proxy-register-exec proxy-ip-or-addr proxy-port idekey multi-session-p
                                          (if (integerp session-port) session-port t)
                                          :session-accept 'dbgp-default-session-accept-p
                                          :session-init 'dbgp-default-session-init
                                          :session-filter 'dbgp-default-session-filter
                                          :session-sentinel 'dbgp-default-session-sentinel)))
    (and (called-interactively-p 'interacive)
         (consp result)
         (message (cdr result)))
    result))

;;;###autoload
(defun dbgp-proxy-register-exec (ip-or-addr port idekey multi-session-p session-port &rest session-params)
  "Register a new DBGp listener to an external DBGp proxy.
The proxy should be found at IP-OR-ADDR / PORT.
This create a new DBGp listener and register it to the proxy
associating with the IDEKEY.
MULTI-SESSION-P indicates if multiple sessions are running or not.
SESSION-PORT is either the integer port number, or t.
SESSION-PARAMS are added to the listener process."
  (cl-block dbgp-proxy-register-exec
    ;; check whether the proxy listener already exists
    (let ((listener (cl-find-if (lambda (listener)
                                  (let ((proxy (dbgp-proxy-get listener)))
                                    (and proxy
                                         (equal ip-or-addr (plist-get proxy :addr))
                                         (eq port (plist-get proxy :port))
                                         (equal idekey (plist-get proxy :idekey)))))
                                dbgp-listeners)))
      (if listener
          (cl-return-from dbgp-proxy-register-exec
            (cons listener
                  (format "The DBGp proxy listener has already been started. idekey: %s" idekey)))))

    ;; send commands to the external proxy instance
    (let* ((listener-proc (make-network-process :name "DBGp proxy listener"
                                                :server t
                                                :service (if (and (numberp session-port) (< 0 session-port))
                                                             session-port
                                                           t)
                                                :family 'ipv4
                                                :noquery t
                                                :filter 'dbgp-comint-setup
                                                :sentinel 'dbgp-listener-sentinel))
           (listener-port (cl-second (process-contact listener-proc)))
           (result (dbgp-proxy-send-command ip-or-addr port
                                            (format "proxyinit -a %s:%s -k %s -m %d"
                                                    dbgp-local-address listener-port idekey
                                                    (if multi-session-p 1 0)))))
      (if (and (consp result)
               (not (equal "1" (xml-get-attribute result 'success))))
          ;; successfully connected to the proxy, but respond an error.
          ;; try to send another command.
          (setq result (dbgp-proxy-send-command ip-or-addr port
                                                (format "proxyinit -p %s -k %s -m %d"
                                                        listener-port idekey
                                                        (if multi-session-p 1 0)))))
      (when (not (and (consp result)
                      (equal "1" (xml-get-attribute result 'success))))
        ;; connection failed or the proxy respond an error.
        ;; give up.
        (dbgp-process-kill listener-proc)
        (cl-return-from dbgp-proxy-register-exec
          (if (not (consp result))
              (cons result
                    (cond
                     ((eq :proxy-not-found result)
                      (format "Cannot connect to DBGp proxy \"%s:%s\"." ip-or-addr port))
                     ((eq :no-response result)
                      "DBGp proxy responds no message.")
                     ((eq :invalid-xml result)
                      "DBGp proxy responds with invalid XML.")
                     (t (symbol-name result))))
            (cons :error-response
                  (format "DBGp proxy returns an error: %s"
                          (dbgp-xml-get-error-message result))))))

      ;; well done.
      (dbgp-plist-put listener-proc :proxy (list :addr ip-or-addr
                                                 :port port
                                                 :idekey idekey
                                                 :multi-session multi-session-p))
      (dbgp-plist-put listener-proc :listener listener-proc)
      (and session-params
           (nconc (process-plist listener-proc) session-params))
      (setq dbgp-listeners (cons listener-proc dbgp-listeners))
      (cons listener-proc
            (format "New DBGp proxy listener is registered. idekey: `%s'" idekey)))))

;;;###autoload
(defun dbgp-proxy-unregister (idekey &optional proxy-ip-or-addr proxy-port)
  "Unregister the DBGp listener associated with IDEKEY from a DBGp proxy.
After unregistration, it kills the listener instance.
PROXY-IP-OR-ADDR is the ip or host address of the proxy instance.
PROXY-PORT is the port number."
  (interactive
   (let (proxies idekeys idekey)
     ;; collect idekeys.
     (mapc (lambda (listener)
             (let ((proxy (dbgp-proxy-get listener)))
               (and proxy
                    (setq proxies (cons listener proxies))
                    (add-to-list 'idekeys (plist-get proxy :idekey)))))
           dbgp-listeners)
     (or proxies
         (error "No DBGp proxy listener exists."))
     ;; ask user for the target idekey.
     (setq idekey (completing-read "IDE key: " idekeys nil t
                                   (and (eq 1 (length idekeys))
                                        (car idekeys))))
     ;; filter proxies and leave ones having the selected ideky.
     (setq proxies (cl-remove-if (lambda (proxy)
                                   (not (equal idekey (plist-get (dbgp-proxy-get proxy) :idekey))))
                                 proxies))
     (let ((proxy (if (= 1 (length proxies))
                      ;; solo proxy.
                      (car proxies)
                    ;; two or more proxies has the same ideky.
                    ;; ask user to select a proxy unregister from.
                    (let* ((addrs (mapcar (lambda (proxy)
                                            (let ((prop (dbgp-proxy-get proxy)))
                                              (format "%s:%s" (plist-get prop :addr) (plist-get prop :port))))
                                          proxies))
                           (addr (completing-read "Proxy candidates: " addrs nil t (car addrs)))
                           (pos (cl-position addr addrs)))
                      (and pos
                           (nth pos proxies))))))
       (list idekey
             (plist-get (dbgp-proxy-get proxy) :addr)
             (plist-get (dbgp-proxy-get proxy) :port)))))

  (let* ((proxies
          (remq nil
                (mapcar (lambda (listener)
                          (let ((prop (dbgp-proxy-get listener)))
                            (and prop
                                 (equal idekey (plist-get prop :idekey))
                                 (or (not proxy-ip-or-addr)
                                     (equal proxy-ip-or-addr (plist-get prop :addr)))
                                 (or (not proxy-port)
                                     (equal proxy-port (plist-get prop :port)))
                                 listener)))
                        dbgp-listeners)))
         (proxy (if (< 1 (length proxies))
                    (error "Multiple proxies are found. Needs more parameters to determine for unregistration.")
                  (car proxies)))
         (result (and proxy
                      (dbgp-proxy-unregister-exec proxy)))
         (status (cons result
                       (cond
                        ((processp result)
                         (format "The DBGp proxy listener of `%s' is unregistered." idekey))
                        ((null result)
                         (format "DBGp proxy listener of `%s' is not registered." idekey))
                        ((stringp result)
                         (format "DBGp proxy returns an error: %s" result))
                        ((eq :proxy-not-found result)
                         (format "Cannot connect to DBGp proxy \"%s:%s\"." proxy-ip-or-addr proxy-port))
                        ((eq :no-response result)
                         "DBGp proxy responds no message.")
                        ((eq :invalid-xml result)
                         "DBGp proxy responds with invalid XML.")))))
    (and (called-interactively-p 'interactive)
         (cdr status)
         (message (cdr status)))
    status))

;;;###autoload
(defun dbgp-proxy-unregister-exec (proxy)
  "Unregister PROXY from a DBGp proxy.
After unregistration, it kills the listener instance."
  (with-temp-buffer
    (let* ((prop (dbgp-proxy-get proxy))
           (result (dbgp-proxy-send-command (plist-get prop :addr)
                                            (plist-get prop :port)
                                            (format "proxystop -k %s" (plist-get prop :idekey)))))
      ;; no matter of the result, remove proxy listener from the dbgp-listeners list.
      (dbgp-process-kill proxy)
      (if (consp result)
          (or (equal "1" (xml-get-attribute result 'success))
              (dbgp-xml-get-error-message result))
        result))))

(defun dbgp-sessions-kill-all ()
  (interactive)
  (mapc 'delete-process dbgp-sessions)
  (setq dbgp-sessions nil))

;;--------------------------------------------------------------
;; DBGp listener functions
;;--------------------------------------------------------------

(defun dbgp-proxy-send-command (addr port string)
  "Send DBGp proxy command string to an external DBGp proxy.
ADDR and PORT is the address of the target proxy.
This function returns an xml list if the command succeeds,
or a symbol: `:proxy-not-found', `:no-response', or `:invalid-xml'.
STRING is a string, the command sent into the process."
  (with-temp-buffer
    (let ((proc (ignore-errors
                  (make-network-process :name "DBGp proxy negotiator"
                                        :buffer (current-buffer)
                                        :host addr
                                        :service port
                                        :sentinel (lambda (proc string) ""))))
          xml)
      (if (null proc)
          :proxy-not-found
        (process-send-string proc string)
        (dotimes (x 50)
          (if (= (point-min) (point-max))
              (sit-for 0.1 t)))
        (if (= (point-min) (point-max))
            :no-response
          (or (ignore-errors
                (setq xml (car (xml-parse-region (point-min) (point-max)))))
              :invalid-xml))))))

(defun dbgp-listener-alive-p (port)
  "Return t if any listener for PORT is alive."
  (let ((listener (dbgp-listener-find port)))
    (and listener
         (eq 'listen (process-status listener)))))

;;--------------------------------------------------------------
;; DBGp listener process log and sentinel
;;--------------------------------------------------------------

(defun dbgp-listener-sentinel (proc string)
  (with-current-buffer (get-buffer-create "*DBGp Listener*")
    (insert (format "[SNT] %S %s\n" proc string)))
  (setq dbgp-listeners (remq proc dbgp-listeners)))

(defun dbgp-listener-log (&rest arg)
  (with-current-buffer (get-buffer-create "*DBGp Listener*")
    (insert (format "[LOG] %S\n" arg))))

;;--------------------------------------------------------------
;; DBGp session process filter and sentinel
;;--------------------------------------------------------------

(defvar dbgp-filter-defer-flag nil
  "Non-nil means don't process anything from the debugger right now.
It is saved for when this flag is not set.")
(defvar dbgp-filter-defer-faced nil
  "Non-nil means this is text that has been saved for later in `gud-filter'.")
(defvar dbgp-filter-pending-text nil
  "Non-nil means this is text that has been saved for later in `gud-filter'.")
(defvar dbgp-delete-prompt-marker nil)
(defvar dbgp-filter-input-list nil)

(defvar dbgp-buffer-process nil
  "The DBGp buffer process.")
(put 'dbgp-buffer-process 'permanent-local t)


;; (defadvice open-network-stream (around debugclient-pass-process-to-comint last)
;;   "[comint hack] Pass the spawned DBGp client process to comint."
;;   (let* ((buffer (ad-get-arg 1))
;; 	 (proc (buffer-local-value 'dbgp-buffer-process buffer)))
;;     (if proc (progn (set-process-buffer proc buffer)
;; 		    (setq ad-return-value proc)))))


(defun dbgp-comint-setup (proc string)
  "Setup a new comint buffer for a newly created session process PROC.
This is the first filter function for a new session process created by a
listener process.  After the setup is done, `dbgp-session-filter' function
takes over the filter.
STRING would be the command, but doesn't appear to be used."
  (if (not (dbgp-session-accept-p proc))
      ;; multi session is disabled
      (when (memq (process-status proc) '(run connect open))
        ;; refuse this session
        (set-process-filter proc nil)
        (set-process-sentinel proc nil)
        (process-send-string proc "run -i 1\0")
        (dotimes (i 50)
          (and (eq 'open (process-status proc))
               (sleep-for 0 1)))
        (dbgp-process-kill proc))
    ;; accept
    (setq dbgp-sessions (cons proc dbgp-sessions))
    ;; initialize sub process
    (set-process-query-on-exit-flag proc nil)

    (let* ((listener (dbgp-listener-get proc))
           (buffer-name (format "DBGp <%s:%s>"
                                (cl-first (process-contact proc))
                                (cl-second (process-contact listener))))
           (buf (or (cl-find-if (lambda (buf)
                                  ;; find reusable buffer
                                  (let ((proc (get-buffer-process buf)))
                                    (and (buffer-local-value 'dbgp-buffer-process buf)
                                         (not (and proc
                                                   (eq 'open (process-status proc)))))))
                                (buffer-list))
                    (get-buffer-create buffer-name))))
      (with-current-buffer buf
        (rename-buffer buffer-name)
        ;; store PROC to `dbgp-buffer-process'.
        ;; later the adviced `open-network-stream' will pass it
        ;; comint.
        (set (make-local-variable 'dbgp-buffer-process) proc)
        (set (make-local-variable 'dbgp-filter-defer-flag) nil)
        (set (make-local-variable 'dbgp-filter-defer-faced) nil)
        (set (make-local-variable 'dbgp-filter-input-list) nil)
        (set (make-local-variable 'dbgp-filter-pending-text) nil))
      ;; setup comint buffer
      (ad-enable-advice 'open-network-stream 'around 'debugclient-pass-process-to-comint)
      (ad-activate 'open-network-stream)
      (unwind-protect
          (make-comint-in-buffer "DBGp-Client" buf (cons t t))
        (ad-deactivate 'open-network-stream)
        (ad-disable-advice 'open-network-stream 'around 'debugclient-pass-process-to-comint))
      ;; update PROC properties
      (set-process-filter proc #'dbgp-session-filter)
      (set-process-sentinel proc #'dbgp-session-sentinel)
      (with-current-buffer buf
        (set (make-local-variable 'dbgp-delete-prompt-marker)
             (make-marker))
        ;;(set (make-local-variable 'comint-use-prompt-regexp) t)
        ;;(setq comint-prompt-regexp (concat "^" dbgp-command-prompt))
        (setq comint-input-sender 'dbgp-session-send-string)
        ;; call initializer function
        (funcall (or (dbgp-plist-get listener :session-init)
                     'null)
                 proc))
      (dbgp-session-filter proc string))))

(defun dbgp-session-accept-p (proc)
  "Determine whether PROC should be accepted to be a new session."
  (let ((accept-p (dbgp-plist-get proc :session-accept)))
    (or (not accept-p)
        (funcall accept-p proc))))

(defun dbgp-session-send-string (proc string &optional echo-p)
  "Send to process PROC a DBGp protocol STRING.
If ECHO-P is t, echo the input as well."
  (if echo-p
      (dbgp-session-echo-input proc string))
  (comint-send-string proc (concat string "\0")))

(defun dbgp-session-echo-input (proc string)
  (with-current-buffer (process-buffer proc)
    (if dbgp-filter-defer-flag
        (setq dbgp-filter-input-list
              (append dbgp-filter-input-list (list string)))
      (let ((eobp (eobp))
            (process-window (get-buffer-window (current-buffer))))
        (save-excursion
          (save-restriction
            (widen)
            (goto-char (process-mark proc))
            (insert (propertize
                     (concat string "\n")
                     'front-sticky t
                     'font-lock-face 'comint-highlight-input))
            (set-marker (process-mark proc) (point))))
        (when eobp
          (if process-window
              (with-selected-window process-window
                (goto-char (point-max)))
            (goto-char (point-max))))))))

(defun dbgp-session-filter (proc string)
  "Given process PROC and string STRING, this is where the actual buffer insertion is done."
  (let ((buf (process-buffer proc))
        (listener (dbgp-listener-get proc))
        (session-filter (dbgp-plist-get proc :session-filter))
        output process-window chunks)
    (cl-block dbgp-session-filter
      (unless (buffer-live-p buf)
        (cl-return-from dbgp-session-filter))

      (with-current-buffer buf
        (when dbgp-filter-defer-flag
          ;; If we can't process any text now,
          ;; save it for later.
          (setq dbgp-filter-defer-faced t
                dbgp-filter-pending-text (if dbgp-filter-pending-text
                                             (concat dbgp-filter-pending-text string)
                                           string))
          (cl-return-from dbgp-session-filter))

        ;; If we have to ask a question during the processing,
        ;; defer any additional text that comes from the debugger
        ;; during that time.
        (setq dbgp-filter-defer-flag t)
        (setq dbgp-filter-defer-faced nil)

        (ignore-errors
          ;; Process now any text we previously saved up.
          (setq dbgp-filter-pending-text (if dbgp-filter-pending-text
                                             (concat dbgp-filter-pending-text string)
                                           string))
          (setq chunks (dbgp-session-response-to-chunk))

          ;; If we have been so requested, delete the debugger prompt.
          (if (marker-buffer dbgp-delete-prompt-marker)
              (save-restriction
                (widen)
                (let ((inhibit-read-only t))
                  (delete-region (process-mark proc)
                                 dbgp-delete-prompt-marker)
                  (comint-update-fence)
                  (set-marker dbgp-delete-prompt-marker nil))))

          ;; Save the process output, checking for source file markers.
          (and chunks
               (setq output
                     (concat
                      (mapconcat (if (functionp session-filter)
                                     (lambda (chunk) (funcall session-filter proc chunk))
                                   #'quote)
                                 chunks
                                 "\n")
                      "\n"))
               (setq output
                     (concat output
                             (if dbgp-filter-input-list
                                 (mapconcat (lambda (input)
                                              (concat
                                               (propertize dbgp-command-prompt
                                                           'font-lock-face 'comint-highlight-prompt)
                                               (propertize (concat input "\n")
                                                           'font-lock-face 'comint-highlight-input)))
                                            dbgp-filter-input-list
                                            "")
                               dbgp-command-prompt)))
               (setq dbgp-filter-input-list nil))))
      ;; Let the comint filter do the actual insertion.
      ;; That lets us inherit various comint features.
      (and output
           (ignore-errors
             (comint-output-filter proc output))))
    (when (and (buffer-live-p buf)
               (with-current-buffer buf
                 (setq dbgp-filter-defer-flag nil)
                 dbgp-filter-defer-faced))
      (dbgp-session-filter proc ""))))

(defun dbgp-session-response-to-chunk ()
  (let* ((string dbgp-filter-pending-text)
         (send (length string))		; string end
         (lbeg 0)			; line begin
         tbeg				; text begin
         tlen				; text length
         (i 0)				; running pointer
         chunks)
    (while (< i send)
      (if (< 0 (elt string i))
          (cl-incf i)
        (setq tlen (string-to-number (substring string lbeg i)))
        (setq tbeg (1+ i))
        (setq i (+ tbeg tlen))
        (when (< i send)
          (setq chunks (cons (substring string tbeg i) chunks))
          (cl-incf i)
          (setq lbeg i))))
    ;; Remove chunk from `dbgp-filter-pending-text'.
    (setq dbgp-filter-pending-text
          (and (< lbeg i)
               (substring dbgp-filter-pending-text lbeg)))
    (nreverse chunks)))

(defun dbgp-session-sentinel (proc string)
  (let ((sentinel (dbgp-plist-get proc :session-sentinel)))
    (ignore-errors
      (and (functionp sentinel)
           (funcall sentinel proc string))))
  (setq dbgp-sessions (remq proc dbgp-sessions)))

;;--------------------------------------------------------------
;; default session initializer, filter and sentinel
;;--------------------------------------------------------------

(defun dbgp-default-session-accept-p (proc)
  "Determine whether PROC should be accepted to be a new session."
  (or (not dbgp-sessions)
      (if (dbgp-proxy-p proc)
          (plist-get (dbgp-proxy-get proc) :multi-session)
        (dbgp-plist-get proc :multi-session))))

(defun dbgp-default-session-init (proc)
  (with-current-buffer (process-buffer proc)
    (pop-to-buffer (current-buffer))))

(defun dbgp-default-session-filter (proc string)
  (with-temp-buffer
    ;; parse xml
    (insert (replace-regexp-in-string "\n" "" string))
    (let ((xml (car (xml-parse-region (point-min) (point-max))))
          text)
      ;; if the xml has a child node encoded with base64, decode it.
      (when (equal "base64" (xml-get-attribute xml 'encoding))
        ;; remain decoded string
        (setq text (with-current-buffer (process-buffer proc)
                     (decode-coding-string
                      (base64-decode-string (car (xml-node-children xml)))
                      buffer-file-coding-system)))
        ;; decoded string may have invalid characters for xml,
        ;; so replace the child node with a placeholder
        (setcar (xml-node-children xml) "\0"))

      ;; create formatted xml string
      (erase-buffer)
      (when (string-match "^.*?\\?>" string)
        (insert (match-string 0 string))
        (insert "\n"))
      (xml-print (list xml))
      (add-text-properties (point-min)
                           (point-max)
                           (list 'front-sticky t
                                 'font-lock-face 'dbgp-response-face))
      (when text
        ;; restore decoded string into a right place
        (goto-char (point-min))
        (and (search-forward "\0" nil t)
             (replace-match (propertize (concat "\n" text)
                                        'front-sticky t
                                        'font-lock-face 'dbgp-decoded-string-face)
                            nil t)))
      ;; return a formatted xml string
      (buffer-string))))

(defun dbgp-default-session-sentinel (proc string)
  (let ((output "\nDisconnected.\n\n"))
    (when (buffer-live-p (process-buffer proc))
      (dbgp-session-echo-input proc output))))


(defadvice open-network-stream (around debugclient-pass-process-to-comint )
  "[comint hack] Pass the spawned DBGp client process to comint."
  (let* ((buffer (ad-get-arg 1))
         (proc (buffer-local-value 'dbgp-buffer-process buffer)))
    (set-process-buffer proc buffer)
    (setq ad-return-value proc) ))

;;(ad-unadvise 'open-network-stream)
;;(ad-deactivate 'open-network-stream  )
(ad-disable-advice 'open-network-stream 'around 'debugclient-pass-process-to-comint)

(provide 'dbgp)

;;; dbgp.el ends here
