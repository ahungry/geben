;;; gud-xdebug.el --- debugging functions with Xdebug upon gud.
;; 
;; Filename: gud-xdebug.el
;; Description: 
;; Author: thoney <thoney@nakarika.com>
;; Maintainer: thoney <thoney@nakarika.com>
;; Created: Fri Sep 22 18:44:25 2006
;; Version: 0.01
;; Last-Updated: Mon Dec 25 16:26:09 2006 (32400 JST)
;;           By: thoney <thoney@nakarika.com>
;;     Update #: 65
;; URL: http://sourceforge.net/projects/geben/
;; Keywords: debugger
;; Compatibility: 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;;
;; This file is part of GEBEN.
;; 
;; Xdebug protocol handlers for the `gud' mode.
;; 
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

(require 'gud)
(require 'xml)

(defcustom gud-xdebug-port 9000
  "Port number for connecting to a xdebug session"
  :type 'integer
  :group 'gud)

(defcustom gud-xdebug-process-dbgp-hook nil
  "Functions to call when xdebug received a dbgp protocol message.
Functions is called with an argument XML which is XMLized message."
  :type 'hook
  :group 'gud)

(defcustom gud-xdebug-visiting-file-hook nil
  "Functions to call when the sessoin visits a source code file.
Functions is called with an argument BUFFER which is visiting source code buffer."
  :type 'hook
  :group 'gud)

;;; History of argument lists passed to xdebug.
(defvar gud-xdebug-history nil)

(defun gud-xdebug-massage-args (file args) args)

(defun gud-xdebug-parse-output (string)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char comint-last-output-start)
      ;; Process all the complete markers in this chunk.
      (while (search-forward "<?xml" nil t)
	(let* ((beg (match-beginning 0))
	       (end (if (search-forward "\n(cmd)")
			(1+ (match-beginning 0))
		      (point-max)))
	       (xml (ignore-errors (xml-parse-region beg end))))
	  (when xml
	      (gud-xdebug-process-chunk xml))
	  (goto-char end))))))

(defun gud-xdebug-process-chunk (xml)
  (run-hook-with-args 'gud-xdebug-process-dbgp-hook (car-safe xml)))

;; There's no guarantee that Emacs will hand the filter the entire
;; marker at once; it could be broken up across several strings.  We
;; might even receive a big chunk with several markers in it.  If we
;; receive a chunk of text which looks like it might contain the
;; beginning of a marker, we save it here between calls to the
;; filter.
(defun gud-xdebug-marker-filter (string)
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
      (mapc #'gud-xdebug-process-chunk xml-list))
    output))

(defun gud-xdebug-find-file (f)
  (let ((buffer (find-file-noselect f)))
    (when buffer
      (run-hook-with-args 'gud-xdebug-visiting-file-hook buffer))
    buffer))

(defcustom gud-xdebug-command-name "debugclient"
  "*Path for the Xdebug client program.
Through this program `gud-xdebug' communicates with debuggee servers."
  :type 'string
  :group 'gud)

;;;###autoload
(defun xdebug (&optional port)
  "Run a xdebug client in buffer *gud-<port number>*.
PORT is TCP port number with which the xdebug client waits for a
xdebug session connection.
If PORT is nil, the value of `gud-xdebug-port' is used.
Otherwise the specified PORT number is applied."
  (interactive "P")
  (unless (numberp port)
    (if (numberp gud-xdebug-port)
	(setq port gud-xdebug-port)
      (error "`gud-xdebug-port' is not a number but a %s" (type-of gud-xdebug-port))))
  (save-window-excursion
    (when gud-comint-buffer
      (kill-buffer gud-comint-buffer))
    (gud-common-init (format "%s -p %d" gud-xdebug-command-name gud-xdebug-port)
		     'gud-xdebug-massage-args
		     'gud-xdebug-marker-filter 'gud-xdebug-find-file)
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
    ;;  (add-hook 'comint-output-filter-functions 'gud-xdebug-parse-output t t)
    (run-hooks 'xdebug-mode-hook))
  (message "xdebug started."))


(provide 'gud-xdebug)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; gud-xdebug.el ends here
