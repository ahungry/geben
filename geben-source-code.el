;;; geben-source-code.el --- 
;; 
;; Filename: geben-source-code.el
;; Description: 
;; Author: thoney <thoney@nakarika.com>
;; Maintainer: thoney <thoney@nakarika.com>
;; Created: Mon Dec  4 10:13:54 2006
;; Version: 0.01
;; Last-Updated: Mon Dec 25 16:22:17 2006 (32400 JST)
;;           By: thoney <thoney@nakarika.com>
;;     Update #: 35
;; URL: http://sourceforge.net/projects/geben/
;; Keywords: php, debugger
;; Compatibility: 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;; Thie file is part of GEBEN.
;; Source code file classes implementations.
;; Each of the class' object represents a source code file.
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

(defcustom geben-source-code-after-visit-hook nil
  "*Hook running after the visiting of a debuggee source code buffer."
  :group 'geben
  :type 'hook)

(defcustom geben-source-code-before-leave-hook nil
  "*Hook running before the leaving of a debuggee source code buffer."
  :group 'geben
  :type 'hook)

;;;
;;; <geben-source-code> class
;;;

(defclass <geben-source-code> ()
  ((path :initarg :path
	 :initform ""
	 :type string
	 :documentation
	 "Full path to locally stored source code file.")
   (fileuri :initarg :fileuri
	    :initform ""
	    :type string
	    :documentation
	    "File URI for the source code file mapped with this instance."))
  :documentation
  "Represent a source code file. An abstract class.")

(defmethod destructor ((this <geben-source-code>) &rest params)
  "Destructor for cleaning up any dynamic links to our object.
Argument THIS is the object being destroyed.  PARAMS are additional
ignored parameters."
  (let ((buf (find-buffer-visiting (oref this path))))
    (when buf
      (run-hook-with-args 'geben-source-code-before-leave-hook buf))))

(defmethod geben-visit-file ((this <geben-source-code>))
  "Visit to a local source code file."
  (let ((buf (find-file (oref this path))))
    (run-hook-with-args 'geben-source-code-after-visit-hook buf)
    buf))

;;;
;;; <geben-local-source-code> class
;;;

(defclass <geben-local-source-code> (<geben-source-code>)
  ()
  :documentation
  "Represent a source code file. This object deals with FILEURI as a
local file and opens it directly(cf. `<geben-remote-source-code>').
Used when FILEURI and PATH can be assumed as the same file.")

;;;
;;; <geben-remote-source-code> class
;;;

(defclass <geben-remote-source-code> (<geben-source-code>)
  ()
  :documentation
  "Represent a source code file. This object deals with FILEURI as a
remote file. When it opens the file it reads and stores the file
contents to local storage temporalily and will delete it when closing
a debug session.
Used when FILEURI refers remote site or for a local file can not be
opened of some reasons (under modifications, etc.).")

(defmethod destructor ((this <geben-remote-source-code>) &rest params)
  "Destructor for cleaning up any dynamic links to our object.
Argument THIS is the object being destroyed.  PARAMS are additional
ignored parameters."
  (call-next-method)
  (let ((buf (find-buffer-visiting (oref this path))))
    (when buf
;;	  Not implemented yet
;; 	  (and (buffer-modified-p buf)
;; 	       (switch-to-buffer buf)
;; 	       (yes-or-no-p "Buffer is modified. Save it?")
;; 	       (geben-write-file-contents this buf))
      (kill-buffer buf))))

(defmethod geben-update-file-content ((this <geben-remote-source-code>) path)
  (oset this :path path)
  (let ((buf (find-buffer-visiting path)))
    (when buf
      (with-current-buffer buf
	(revert-buffer)))))

(defmethod geben-write-file-contents ((this <geben-remote-source-code>) buf)
  "Write file contents to remote or local site via ftp/sftp/scp/etc.

Currently not implemented"
  nil)

(defmethod geben-path-equal-p ((this <geben-source-code>) path)
  (string= (file-truename path)
	   (file-truename (oref this path))))
  
(provide 'geben-source-code)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; geben-source-code.el ends here
