;;; geben-ecb.el --- 
;; 
;; Filename: geben-utils.el
;; Description: 
;; Author: thoney <thoney@nakarika.com>
;; Maintainer: thoney <thoney@nakarika.com>
;; Created: Sun Oct  1 11:33:08 2006
;; Version: 0.01
;; Last-Updated: Mon Dec 25 16:08:40 2006 (32400 JST)
;;           By: thoney <thoney@nakarika.com>
;;     Update #: 16
;; URL: http://sourceforge.net/projects/geben/
;; Keywords: php, debugger
;; Compatibility: 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;; This file is part of GEBEN.
;; Trial implementation for GEBEN to work with ECB, Emacs Code Browser.
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


;; ---------------------------------------------------------------------------
;; --- Some requirements we always need if using the ECB layout-engine -------
;; ---------------------------------------------------------------------------

(eval-when-compile
  ;; to avoid compiler grips
  (require 'cl)
  (require 'xml))

(eval-when-compile
  (when (featurep 'ecb)

(require 'ecb-util)
(require 'ecb-layout)
(require 'ecb-common-browser)

;; ---------------------------------------------------------------------------
;; --- Code for the stack tree buffer ----------------------------------------
;; ---------------------------------------------------------------------------

(defconst geben-ecb-stacktree-buffer-name " *geben stack tree*")

(defun geben-ecb-stacktree-update (stack-list)
  (interactive)

  (ecb-do-if-buffer-visible-in-ecb-frame 'geben-ecb-stacktree-buffer-name
    (with-current-buffer (get-buffer geben-ecb-stacktree-buffer-name)
      (tree-buffer-clear-tree)
      (dolist (stack (nreverse stack-list))
	(tree-node-new
	 (xml-substitute-special
	  (format "%s - %s(%s)"
		  (xml-get-attribute stack 'where)
		  (xml-get-attribute stack 'filename)
		  (xml-get-attribute stack 'lineno)))
	 (xml-get-attribute stack 'level)
	 stack t (tree-buffer-get-root)))
      (tree-buffer-update))))

;; Two conveniance-commands for the user

(defun ecb-maximize-stacktree-window ()
  "Maximize the stacktree-window.
I.e. delete all other ECB-windows, so only one ECB-window and the
edit-window\(s) are visible \(and maybe a compile-window). Works
also if the ECB-analyse-window is not visible in current layout."
  (interactive)
  (ecb-maximize-ecb-buffer geben-ecb-stacktree-buffer-name t))

(defun ecb-goto-stacktree-window ()
  "Make the stacktree-window the current window."
  (interactive)
  (ecb-goto-ecb-window geben-ecb-stacktree-buffer-name))


;; The "window-dedicator"-function for the stacktree-buffer. See
;; `defecb-window-dedicator' for an explanation.

(defecb-window-dedicator geben-ecb-set-stacktree-buffer
  geben-ecb-stacktree-buffer-name
  "Set the buffer in the current window to the stacktree-buffer and make this
window dedicated for this buffer."
  (switch-to-buffer ;(get-buffer-create geben-ecb-stacktree-buffer-name))
   geben-ecb-stacktree-buffer-name)
  (setq buffer-read-only t))

(defecb-tree-buffer-creator geben-ecb-create-stacktree-buffer
  geben-ecb-stacktree-buffer-name
  "Create the tree-buffer for stack tree."
  (tree-buffer-create
   geben-ecb-stacktree-buffer-name

   :frame ecb-frame
   :mouse-action-trigger ecb-tree-mouse-action-trigger
   :is-click-valid-fn 'ecb-interpret-mouse-click
   :node-selected-fn 'ecb-tree-buffer-node-select-callback
   :node-expanded-fn 'ecb-tree-buffer-node-expand-callback
   :node-collapsed-fn 'ecb-tree-buffer-node-collapsed-callback
   :node-mouse-over-fn 'ecb-mouse-over-analyse-node
   ;;   :mouse-highlight-fn 'ecb-analyse-node-mouse-highlighted-p
   ;;   :node-data-equal-fn 'ecb-analyse-compare-node-data
   :maybe-empty-node-types nil
   :leaf-node-types nil
   ;;   :menu-creator 'ecb-analyse-menu-creator
   ;;   :menu-titles (ecb-analyse-gen-menu-title-creator)
   :modeline-menu-creator 'ecb-common-tree-buffer-modeline-menu-creator
   :trunc-lines t
   :read-only t
   :tree-indent ecb-tree-indent
   :incr-search-p t
   :incr-search-additional-pattern nil
   :arrow-navigation ecb-tree-navigation-by-arrow
   :hor-scroll-step ecb-tree-easy-hor-scroll
   :default-images-dir (car ecb-tree-image-icons-directories)
   :additional-images-dir (ecb-member-of-symbol/value-list
			   geben-ecb-stacktree-buffer-name
			   (cdr ecb-tree-image-icons-directories)
			   'car 'cdr)
   :image-file-prefix "ecb-"
   :tree-style ecb-tree-buffer-style
   :ascii-guide-face ecb-tree-guide-line-face
   :type-facer nil
   :expand-symbol-before-p ecb-tree-expand-symbol-before
   ;;   :highlight-node-face ecb-analyse-face
   ;;   :general-face ecb-analyse-general-face
   :after-create-hook nil
   :after-update-hook nil))

;; ---------------------------------------------------------------------------
;; --- The layout definition with a stacktree- and a action-buffer -----------
;; ---------------------------------------------------------------------------


(ecb-layout-define "geben-layout1" left
		   "Layout for geben."

		   (ecb-set-directories-buffer)
		   (let ((lines (ecb-split-ver 0.3)))
		     (ecb-set-methods-buffer)
		     (ecb-split-ver lines))

		   ;; dedicating the stacktree window to the stacktree-buffer
					;(geben-ecb-create-stacktree-buffer)
		   (geben-ecb-set-stacktree-buffer)
  
		   ;; creating the action-window
		   ;;(ecb-split-hor 0.75)
  
		   ;; dedicating the action window to the action-buffer
		   ;;(geben-ecb-set-action-buffer)

		   ;; selecting the edit-window
		   (select-window (next-window)))

;; ---------------------------------------------------------------------------
;; --- (De)activating the new layout and the synchronization -----------------
;; ---------------------------------------------------------------------------

;; Code for saving and restoring the state before activation

(defvar geben-ecb-preact-layout nil)
(defvar geben-ecb-preact-windows-height nil)
(defvar geben-ecb-preact-compile-window-height nil)
(defun geben-ecb-preactivation-state(action)
  (cond ((equal action 'save)
         (setq geben-ecb-preact-layout
	       ecb-layout-name
               geben-ecb-preact-windows-height
	       ecb-windows-height
               geben-ecb-preact-compile-window-height
	       ecb-compile-window-height))
        ((equal action 'restore)
         (setq ecb-layout-name
               geben-ecb-preact-layout
               ecb-windows-height
               geben-ecb-preact-windows-height
               ecb-compile-window-height
               geben-ecb-preact-compile-window-height))))

(defun geben-ecb-activate ()
  "Activate the new layout \"geben-layout1\".
Add `geben-ecb-stacktree-sync' to `ecb-current-buffer-sync-hook', set
`ecb-compile-window-height' to 5 and `ecb-windows-height' to 6. The
preactivation-state is saved and will be restored by
`geben-ecb-deactivate'."
  (interactive)
  (when (and (featurep 'ecb)
	     ecb-minor-mode
	     (equal (selected-frame) ecb-frame)
	     (not (ecb-string= ecb-layout-name "geben-layout1")))
    ;; activating the synchronization of the stacktree-window
;;     (add-hook 'ecb-current-buffer-sync-hook
;; 	      'geben-ecb-stacktree-sync)
    ;; saving the state
    (geben-ecb-preactivation-state 'save)
    ;; make sure the tree buffers to be created
    (ecb-tree-buffer-creators-run)
    ;; switch to our prefered layout
    (setq ecb-windows-height 6)
    ;;(setq ecb-compile-window-height 8)
    (let ((ecb-change-layout-preserves-compwin-state nil))
      (ecb-layout-switch "geben-layout1"))))

;; Deactivation of the example

(defun geben-ecb-deactivate ()
  "Deactivate the new layout \"geben-layout1\".
Remove `geben-ecb-stacktree-sync' from `ecb-current-buffer-sync-hook' and
restore the state as before activation."
  (interactive)
  (when (and (featurep 'ecb)
	     ecb-minor-mode
	     (ecb-string= ecb-layout-name "geben-layout1"))
;;    (remove-hook 'ecb-current-buffer-sync-hook
;;		 'geben-ecb-stacktree-sync)
    (let ((old-frame (selected-frame)))
      (select-frame ecb-frame)
      (unwind-protect
	  (progn
	    (geben-ecb-preactivation-state 'restore)
	    (ecb-layout-switch ecb-layout-name))
	(select-frame old-frame)))))
)) 					;;(eval-when-compile (when (featurep 'ecb)

(eval-when-compile
  (unless (featurep 'ecb)
    (defun geben-make-dummy-fn (sym)
      `(defun ,sym nil)
      (fset sym (lambda (&rest args) t)))
    (mapc (lambda (fn)
	    (geben-make-dummy-fn fn))
	  '(geben-ecb-activate
	    geben-ecb-deactivate
	    geben-ecb-stacktree-update))))


(provide 'geben-ecb)

;; geben-ecb.el ends here
