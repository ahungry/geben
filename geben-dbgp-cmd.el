;;; geben-dbgp-command.el --- 
;; 
;; Filename: geben-dbgp-cmd.el
;; Description: 
;; Author: thoney <thoney@nakarika.com>
;; Maintainer: thoney <thoney@nakarika.com>
;; Created: Tue Dec 12 20:14:09 2006
;; Version: 0.01
;; Last-Updated: Mon Dec 25 16:04:29 2006 (32400 JST)
;;           By: thoney <thoney@nakarika.com>
;;     Update #: 17
;; URL: http://sourceforge.net/projects/geben/
;; Keywords: php, debugger
;; Compatibility: 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
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

(require 'eieio)

(defclass <geben-dbgp-cmd> ()
  ((operand :initarg :operand
	    :type string
	    :initform ""
	    :documentation
	    "Command operand")
   (params :initarg :params
	   :type (or string list)
	   :initform ""
	   :documentation
	   "Command parameter(s)"))
  "Represents a command for dbgp protocol.")

(defmethod geben-get-operand ((this <geben-dbgp-cmd>))
  (oref this operand))

(defmethod geben-get-params ((this <geben-dbgp-cmd>))
  (let ((params (oref this params)))
    (if (stringp params)
	(list params)
      params)))

(defmethod geben-get-param-arg ((this <geben-dbgp-cmd>) flag)
  (cdr-safe (assoc flag (geben-get-params this))))

(defmethod geben-build-send-command ((this <geben-dbgp-cmd>) tid)
  "Build a send command string for dbgp protocol."
  (mapconcat #'(lambda (x)
		 (cond ((stringp x) x)
		       ((integerp x) (int-to-string x))
		       ((atom (format "%S" x)))
		       ((null x) "")
		       (t x)))
	     (geben-flatten (list (geben-get-operand this)
				  "-i" tid
				  (geben-get-params this)))
	     " "))
  
(provide 'geben-dbgp-cmd)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; geben-dbgp-cmd.el ends here
