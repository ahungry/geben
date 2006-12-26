;;; geben.el --- PHP source code level debugger
;; 
;; Filename: geben.el
;; Description:
;; Author: thoney <thoney@nakarika.com>
;; Maintainer: thoney <thoney@nakarika.com>
;; Created: Sat Sep 30 21:25:36 2006
;; Version: 0.01 sample implementation
;; Last-Updated: Tue Dec 26 17:58:54 2006 (32400 JST)
;;           By: thoney <thoney@nakarika.com>
;;     Update #: 179
;; URL: http://sourceforge.net/projects/geben/
;; Keywords: php, debugger
;; Compatibility:
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary:
;;
;; GEBEN is a PHP source code level debugger.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Requirements:
;;
;; [PHP Server side]
;; - non-windows server
;; - Xdebug 2.0 RC2 enabled
;;    http://xdebug.org/
;;
;; [Client side]
;; - Emacs 21.4 and later
;; - xdebugclient
;;    http://xdebug.org/
;; - cedet1.0 pre3
;;   http://cedet.sourceforge.net/
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

;; From custom web page for compatibility between versions of custom
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
	(` (defvar (, var) (, value) (, doc)))))))

(defgroup geben nil
  "A PHP Debugging environment."
  :group 'debug)

(eval-when-compile
  (require 'gud-xdebug)
  (require 'eieio)
  (require 'geben-utils)
  (require 'geben-bp)
  (require 'geben-session)
  (require 'geben-dbgp)
  (require 'geben-mode)
  (require 'geben-source-code)
  (require 'geben-ecb))

(eval-when-compile
  (load-library "cl-seq"))

(add-hook 'kill-emacs-hook
	  (lambda ()
	    (when (geben-current-session)
	      (destructor (geben-current-session)))))

(add-hook 'geben-session-starting-hook
	  (lambda (session)
	    (geben-ecb-activate)))

(add-hook 'geben-session-finished-hook
	  (lambda (session)
	    (ignore-errors
	      (geben-ecb-deactivate)
	      (maphash (lambda (fileuri source-code)
			 (let ((buf (find-buffer-visiting (oref source-code path))))
			   (when buf
			     (with-current-buffer buf
			       (geben-mode nil)))))
		       (oref session source-files)))))

(add-hook 'geben-source-code-after-visit-hook
	  (lambda (buf)
	    (with-current-buffer buf
	      (geben-mode t))))

;;; #autoload
(defun geben ()
  "Start a GEBEN session.
Wait for a session connection with the port number `gud-xdebug-port'.
When a session is connected you can debug the target source code
through buffers under the minor mode of `geben-mode'."
  (interactive)
  (add-hook 'gud-xdebug-process-dbgp-hook #'geben-dbgp-entry)
  (xdebug))

(defun geben-temp-dir ()
  "Get temporary directory to store files used among a GEBEN session."
  (let ((base-dir (expand-file-name "emacs-geben" temporary-file-directory)))
    (unless (file-exists-p base-dir)
      (make-directory base-dir t)
      (set-file-modes base-dir 1023))
    (expand-file-name (format "%d" (emacs-pid)) base-dir)))

(defun geben-temp-path-for-fileuri (fileuri)
  "Make temp path string from FILEURI."
  (when (string-match "^file:///?" fileuri)
    (expand-file-name (substring fileuri (match-end 0)) (geben-temp-dir))))

(provide 'geben)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; geben.el ends here
