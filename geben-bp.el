;;; geben-bp.el --- 
;; 
;; Filename: geben-bp.el
;; Description: 
;; Author: thoney <thoney@nakarika.com>
;; Maintainer: thoney <thoney@nakarika.com>
;; Created: Fri Sep 29 16:38:15 2006
;; Version: 0.01
;; Last-Updated: Mon Dec 25 16:04:42 2006 (32400 JST)
;;           By: thoney <thoney@nakarika.com>
;;     Update #: 49
;; URL: http://sourceforge.net/projects/geben/
;; Keywords: php, debugger
;; Compatibility: 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;;
;; This file is part of GEBEN.
;; Breakpoint maker definition and its manipulators.
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
(require 'linemark)

(defvar geben-breakpoints nil
  "List of breakpoints.")

(defface geben-breakpoint-face '((((class color) (background light))
				(:background "#ff8888"))
			       (((class color) (background dark))
				(:background "red3")))
  "*Face used to indicate a breakpoint line."
  :group 'geben)

(defclass <geben-bp-linemark-group> (linemark-group)
  nil
  "Track a group of `geben-bp-linemark-entry'.")

(defclass <geben-bp-linemark-entry> (linemark-entry)
  nil
  "Breakpoint linemarks.")

(defvar geben-breakpoint-markers (linemark-new-group '<geben-bp-linemark-group> "geben-bp")
  "The geben breakpoint group object.")

(defclass <geben-breakpoint> ()
  ((skipcount :initarg :skipcount
	      :initform 0
	      :type integer
	      :documentation
	      "Number of skipping count for breakpoint hits.")
   (id :initarg :id
	     :documentation
	     "User data associates to this breakpoint")
   ;;   (active :initarg :active
   ;;	   :initform t
   ;;	   :type boolean
   ;;	   :documentation
   ;;	   "Specify whether this breakpoint is active."))
   )
  :abstract t
  :documantation
  "A breakpoint")

(defclass <geben-breakpoint-lineno> (<geben-breakpoint>)
  ((fileuri :initarg :fileuri
	    :type string
	    :documentation
	    "File URI to which this breakpoint belongs.")
   (path :initarg :path
	 :type string
	 :documentation
	 "Local filep path in where this breakpoint is set")
   (lineno :initarg :lineno
	   :type integer
	   :documentation
	   "Line number at where this breakpoint is set")
   (linemark :initarg :linemark
	     :type (or nil linemark-entry)
	     :documentation
	     "Linemark."))
  :documentation
  "A breakpoint set in a file.")

(defmethod initialize-instance ((this <geben-breakpoint-lineno>)
				&optional fields)
  "Initialize a new `<geben-breakpoint-lineno>' object."
  (call-next-method)
  (oset this
	linemark
	(linemark-add-entry geben-breakpoint-markers
			    :filename (oref this path)
			    :line (oref this lineno)
			    :face 'geben-breakpoint-face)))

(defmethod destructor ((this <geben-breakpoint-lineno>))
  (when (oref this linemark)
    (linemark-delete (oref this linemark))))
		       
(defmethod linemark-new-entry ((g <geben-bp-linemark-group>) &rest args)
  "Create a new entry for G using init ARGS."
  (let ((f (plist-get args :filename))
	(l (plist-get args :line)))
    (apply '<geben-bp-linemark-entry> (format "%s %d" f l)
	   args)))

(defmethod linemark-display ((e <geben-bp-linemark-entry>) active-p)
  "Set object E to be active or inactive."
  (if active-p
      (with-slots ((file filename)) e
	(if (oref e overlay)
	    ;; Already active
	    nil
          (let ((buffer (get-file-buffer file)))
            (when buffer
	      (save-excursion
		(set-buffer buffer)
		(save-excursion
		  (goto-line (oref e line))
		  (beginning-of-line)
		  (oset e overlay
			(linemark-make-overlay (point)
					       (save-excursion
						 (forward-line) (point))
					       (current-buffer)))
		  (with-slots (overlay) e
		    (linemark-overlay-put overlay 'face (oref e face))
		    (linemark-overlay-put overlay 'obj e)
		    (linemark-overlay-put overlay 'tag 'linemark))))))))
    ;; Not active
    (with-slots (overlay) e
      (if overlay
	  (progn
	    (condition-case nil
		;; During development of linemark programs, this is helpful
		(linemark-delete-overlay overlay)
	      (error nil))
	    (oset e overlay nil))))))

(defun geben-list-breakpoints ()
  "Display geben breakpoints information."
  (interactive)
  (let (lineno
	(obuf (current-buffer)))
    (mapc #'(lambda (bp)
	      (cond
	       ((eq (class-of bp) <geben-breakpoint-lineno>)
		(add-to-list 'lineno bp))))
	  geben-breakpoints)
    (save-excursion
      (with-current-buffer (get-buffer-create "*geben breakpoints*")
	(let ((buffer-read-only nil))
	  (erase-buffer)
	  (when lineno
	    (insert "* Line number breakpoints\n")
	    (mapc (lambda (bp)
		    (insert (format "%s(%d):"
				    (oref bp fileuri)
				    (oref bp lineno)))
		    (let ((skip (oref bp skipcount)))
		      (when (< 0 skip)
			(insert (format " will skip %d hit%s"
					skip
					(if (< 1 skip) "s" "")))))
		    (insert "\n"))
		  (sort lineno (lambda (a b)
				 (or (string< (oref a fileuri)
					      (oref b fileuri))
				     (and (string= (oref a fileuri)
						   (oref b fileuri))
					  (< (oref a lineno)
					     (oref b lineno))))))))
	  (if (and (null lineno))
	      (insert "no breakpoints.\n")))
	(pop-to-buffer (current-buffer))
	(goto-char (point-min))))
    (pop-to-buffer obuf)))

(defmethod geben-display-breakpoints1 ((bp <geben-breakpoint>) fileuri buffer)
  t)

(defmethod geben-display-breakpoints1 ((bp <geben-breakpoint-lineno>) fileuri buffer)
  (when (string= fileuri (oref bp fileuri))
    (with-current-buffer buffer
      (if (slot-boundp bp 'linemark)
	  (progn
	    (linemark-display (oref bp linemark) nil)
	    (linemark-display (oref bp linemark) t))
	(oset bp
	      linemark
	      (linemark-add-entry geben-breakpoint-markers
				  :line (oref bp lineno)
				  :face 'geben-breakpoint-face))))))

 

(defun geben-display-breakpoints (fileuri &optional buffer)
  (unless buffer
    (setq buffer (current-buffer)))
  (mapc (lambda (bp)
	  (geben-display-breakpoints1 bp fileuri buffer))
	geben-breakpoints))

(defun geben-add-lineno-breakpoint (fileuri path lineno id)
  (let ((old-one (geben-find-lineno-breakpoint fileuri lineno)))
    (if old-one
	(oset old-one id id)
      (add-to-list 'geben-breakpoints
		   (<geben-breakpoint-lineno> (format "%s %d" fileuri lineno)
					    :fileuri fileuri
					    :path path
					    :lineno lineno
					    :id id)))))

(defun geben-remove-breakpoint (id)
  (let ((bp (geben-find-breakpoint id)))
    (when bp
      (setq geben-breakpoints (delete bp geben-breakpoints))
      (destructor bp))))		;call explicitly until eieio
					;find the solution to call
					;destructors implicitly.

(defun geben-find-lineno-breakpoint (fileuri lineno)
  (geben-find-breakpoint-if
   (lambda (bp)
     (and (eq '<geben-breakpoint-lineno> (class-of bp))
	  (string= fileuri (oref bp fileuri))
	  (eq lineno (oref bp lineno))))))

(defun geben-find-breakpoint (id)
  (geben-find-breakpoint-if
   (lambda (bp)
     (equal id (oref bp id)))))

(defun geben-find-breakpoint-if (pred)
  (find nil geben-breakpoints
	:test (lambda (key bp)
		(funcall pred bp))))
	

(provide 'geben-bp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; geben-bp.el ends here
