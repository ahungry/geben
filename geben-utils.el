;;; geben-utils.el --- 
;; 
;; Filename: geben-utils.el
;; Description: 
;; Author: thoney <thoney@nakarika.com>
;; Maintainer: thoney <thoney@nakarika.com>
;; Created: Sun Oct  1 11:33:08 2006
;; Version: 0.01
;; Last-Updated: Mon Dec 25 17:24:30 2006 (32400 JST)
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
;; This file is part of GEBEN.
;; Contains some utility functions.
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

(defun geben-flatten (x)
  "Make cons X to a flat list."
  (labels ((rec (x acc)
		(cond ((null x) acc)
		      ((atom x) (cons x acc))
		      (t (rec (car x) (rec (cdr x) acc))))))
    (rec x nil)))

(defun geben-delete-directory-tree (base-path)
  (if (file-directory-p base-path)
      (progn
	(mapc (lambda (name)
		(let ((fullpath (expand-file-name name base-path)))
		  (cond
		   ((equal name ".") t)
		   ((equal name "..") t)
		   ((or (file-symlink-p fullpath)
			(file-regular-p fullpath))
		    (delete-file fullpath))
		   ((file-directory-p fullpath)
		    (geben-delete-directory-tree fullpath)))))
	      (directory-files base-path nil nil t))
	(delete-directory base-path))))

(defun geben-what-line (&optional pos)
  (let ((opoint (or pos (point))))
    (save-excursion
      (save-restriction
	(widen)
	(forward-line 0)
	(1+ (count-lines 1 (point)))))))

(defun geben-make-local-path (fileuri)
  "Make a local path string from FILEURI."
    (let ((local-path (replace-regexp-in-string "^file://" "" fileuri)))
      (when (eq system-type 'windows-nt)
	(require 'url-util)
	(setq local-path (url-unhex-string (substring local-path 1))))
      local-path))

(provide 'geben-utils)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; geben-utils.el ends here
