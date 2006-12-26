;;; geben-session.el --- 
;; 
;; Filename: geben-session.el
;; Description: 
;; Author: thoney <thoney@nakarika.com>
;; Maintainer: thoney <thoney@nakarika.com>
;; Created: Sat Oct 21 16:14:08 2006
;; Version: 0.01
;; Last-Updated: Mon Dec 25 16:20:07 2006 (32400 JST)
;;           By: thoney <thoney@nakarika.com>
;;     Update #: 10
;; URL: http://sourceforge.net/projects/geben/
;; Keywords: php, debugger
;; Compatibility: 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;; This file is part of GEBEN.
;; GEBEN session classes implementations.
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

(defcustom geben-session-starting-hook nil
  "*Hook running at when the debugging session is starting."
  :group 'geben
  :type 'hook)

(defcustom geben-session-finished-hook nil
  "*Hook running at when the debugging session is finished."
  :group 'geben
  :type 'hook)

(defvar geben-current-session nil
  "Current session")

(defun geben-current-session ()
  "Get the current session."
  geben-current-session)

;;
;; <geben-session>
;; 

(defclass <geben-session> ()
  ()
  :documentation
  "A session.")

(defmethod initialize-instance ((this <geben-session>)
				&optional fields)
  (call-next-method)
  (run-hook-with-args 'geben-session-starting-hook this))

(defmethod geben-finished ((this <geben-session>))
  (run-hook-with-args 'geben-session-finished-hook this))

;;
;; <geben-gud-session>
;; 

(defclass <geben-gud-session> (<geben-session>)
  ())

(provide 'geben-session)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; geben-session.el ends here
