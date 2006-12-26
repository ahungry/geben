;;; geben-mode.el --- 
;; 
;; Filename: geben-mode.el
;; Description: 
;; Author: thoney <thoney@nakarika.com>
;; Maintainer: thoney <thoney@nakarika.com>
;; Created: Sat Oct 21 16:15:38 2006
;; Version: 0.01
;; Last-Updated: Mon Dec 25 16:17:52 2006 (32400 JST)
;;           By: thoney <thoney@nakarika.com>
;;     Update #: 15
;; URL: http://sourceforge.net/projects/geben/
;; Keywords: php, debugger
;; Compatibility: 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; This file is part of GEBEN
;; `geben-mode' implementation.
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
  "Minor mode for debugging source code while in geben.
The edebug buffer commands:
\\{geben-mode-map}"
  nil " *debugging*" geben-mode-map
  (setq buffer-read-only geben-mode))
  
(defvar geben-step-type :step-into
  "`geben-step-again' calls apropriate step command on this value.
Value can be one of followings:
:step-into
:step-out
")

(defun geben-step-again ()
  "Do either `geben-step-into' or `geben-step-over' what the last time called.
Default is `geben-step-into'."
  (interactive)
  (case geben-step-type
    (:step-over (geben-step-over))
    (:step-into (geben-step-into))
    (t (geben-step-into))))
     
(defun geben-step-into ()
  (interactive)
  (setq geben-step-type :step-into)
  (geben-command-step-into (geben-current-session)))

(defun geben-step-over ()
  (interactive)
  (setq geben-step-type :step-over)
  (geben-command-step-over (geben-current-session)))

(defun geben-step-out ()
  (interactive)
  (geben-command-step-out (geben-current-session)))

(defun geben-run ()
  (interactive)
  (geben-command-run (geben-current-session)))

(defun geben-stop ()
  (interactive)
  (geben-command-stop (geben-current-session)))

(defun geben-set-breakpoint ()
  (interactive)
  (geben-command-breakpoint-set (geben-current-session)))

(defun geben-unset-breakpoint ()
  (interactive)
  (geben-command-breakpoint-remove (geben-current-session)))

(defvar geben-eval-history nil)

(defun geben-eval-expression (expr)
  "Eval a php expression."
  (interactive
   (progn
     (list (read-from-minibuffer "Eval: "
				 nil nil nil 'geben-eval-history))))
  (geben-command-eval (geben-current-session) expr))

(defun geben-open-file (fileuri)
  "Open a debugger server side file specified by FILEURI.
FILEURI forms like as \`file:///path/to/file\'."
  (interactive "s")
  (geben-command-source (geben-current-session) fileuri))

(provide 'geben-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; geben-mode.el ends here
