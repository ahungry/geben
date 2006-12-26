;;; wisent-php-tags.el --- Php LALR parser for Emacs

;; Copyright (C) 2001, 2002 David Ponce

;; Author: David Ponce <david@dponce.com>
;; Maintainer: David Ponce <david@dponce.com>
;; Created: 15 Dec 2001
;; Keywords: syntax
;; X-RCS: $Id: wisent-php-tags.el,v 1.30 2004/04/29 10:10:53 ponced Exp $

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;

;;; History:
;; 

;;; Code:

(require 'semantic-wisent)
(require 'wisent-php-tags-wy)
;(require 'semantic-php)
(eval-when-compile
  (require 'semantic-util)
  (require 'semantic-ctxt)
  (require 'semantic-imenu)
  (require 'senator)
  (require 'document))

(defconst wisent-php-label-regex
  "[a-zA-Z_\x7f-\xff][a-zA-Z0-9_\x7f-\xff]*")

;;;;
;;;; Simple parser error reporting function
;;;;

(defun wisent-php-parse-error (msg)
  "Error reporting function called when a parse error occurs.
MSG is the message string to report."
;;   (let ((error-start (nth 2 wisent-input)))
;;     (if (number-or-marker-p error-start)
;;         (goto-char error-start)))
  (message msg)
  ;;(debug)
  )

;;;;
;;;; Local context
;;;;

(defvar wisent-in-php nil)

(define-mode-local-override semantic-get-local-variables
  php-mode ()
  "Get local values from a specific context.
Parse the current context for `field_declaration' nonterminals to
collect tags, such as local variables or prototypes.
This function override `get-local-variables'."
  (let ((vars nil)
        ;; We want nothing to do with funny syntaxing while doing this.
        (semantic-unmatched-syntax-hook nil))
    (while (not (semantic-up-context (point) 'function))
      (save-excursion
        (forward-char 1)
        (setq vars
              (append (semantic-parse-region
                       (point)
                       (save-excursion (semantic-end-of-context) (point))
                       'field_declaration
                       0 t)
                      vars))))
    vars))

(defconst semantic-php-number-regexp
  (eval-when-compile
    (concat "\\("
            "\\<[0-9]+[.][0-9]+\\([eE][-+]?[0-9]+\\)?[fFdD]?\\>"
            "\\|"
            "\\<[0-9]+[.][eE][-+]?[0-9]+[fFdD]?\\>"
            "\\|"
            "\\<[0-9]+[.][fFdD]\\>"
            "\\|"
            "\\<[0-9]+[.]"
            "\\|"
            "[.][0-9]+\\([eE][-+]?[0-9]+\\)?[fFdD]?\\>"
            "\\|"
            "\\<[0-9]+[eE][-+]?[0-9]+[fFdD]?\\>"
            "\\|"
            "\\<0[xX][0-9a-fA-F]+[lL]?\\>"
            "\\|"
            "\\<[0-9]+[lLfFdD]?\\>"
            "\\)"
            ))
  "Lexer regexp to match Java number terminals.
Following is the specification of Java number literals.

DECIMAL_LITERAL:
    [1-9][0-9]*
  ;
HEX_LITERAL:
    0[xX][0-9a-fA-F]+
  ;
OCTAL_LITERAL:
    0[0-7]*
  ;
INTEGER_LITERAL:
    <DECIMAL_LITERAL>[lL]?
  | <HEX_LITERAL>[lL]?
  | <OCTAL_LITERAL>[lL]?
  ;
EXPONENT:
    [eE][+-]?[09]+
  ;
FLOATING_POINT_LITERAL:
    [0-9]+[.][0-9]*<EXPONENT>?[fFdD]?
  | [.][0-9]+<EXPONENT>?[fFdD]?
  | [0-9]+<EXPONENT>[fFdD]?
  | [0-9]+<EXPONENT>?[fFdD]
  ;")

;;;;
;;;; Semantic integration of the Php LALR parser
;;;;

;;;###autoload
(defun wisent-php-default-setup ()
  "Hook run to setup Semantic in `php-mode'.
Use the alternate LALR(1) parser."
  (set (make-local-variable 'wisent-in-php) nil)
  (wisent-php-tags-wy--install-parser)
  (setq
   ;; Lexical analysis
   semantic-lex-number-expression semantic-php-number-regexp
   semantic-lex-analyzer 'wisent-php-tags-lexer
   ;; Parsing
   semantic-tag-expand-function 'wisent-php-expand-tag
   ;; Environment
   semantic-imenu-summary-function 'semantic-format-tag-prototype
   imenu-create-index-function 'semantic-create-imenu-index
   semantic-type-relation-separator-character '(".")
   semantic-command-separation-character ";"
   semantic-lex-comment-regex "\\(/\\*\\|//\\|#\\)"
   ;;document-comment-start "/**"
   ;;document-comment-line-prefix " *"
   ;;document-comment-end " */"
   ;; speedbar and imenu buckets name
   semantic-symbol->name-assoc-list-for-type-parts
   ;; in type parts
   '((type     . "Classes")
     (variable . "Variables")
     (function . "Methods"))
   semantic-symbol->name-assoc-list
   ;; everywhere
   (append semantic-symbol->name-assoc-list-for-type-parts
           '((include  . "Includes")
             (package  . "Package")))
   ;; navigation inside 'type children
   senator-step-at-tag-classes '(function variable)
   ))
  ;; Setup phpdoc stuff
  ;;(semantic-php-doc-setup))

(defun wisent-php--move-to-php-beginning ()
  (if (re-search-forward "<[%?]" nil t)
      (cond
       ((or (looking-at "\\(php\\)?$")
	    (looking-at "\\(php\\)?[[:space:]])"))
	(goto-char (match-end 0))
	'T_NONPHP)
       ((or (looking-at "=$")
	    (looking-at "=[[:space:]]"))
	'T_ECHO_BLOCK)
       (t
	(wisent-php--move-to-php-beginning)))
    (goto-char (point-max))
    nil))

(define-lex-regex-analyzer wisent-php-lex-prologue
  "Detect and create a prologue token."
  "<[?%]\\(php\\)?\\([[:space:]]+\\|$\\)"
  ;; Zing to the end of this brace block.
  (let ((start (match-beginning 0))
        (end   (match-end 0)))
    (semantic-lex-push-token
     (semantic-lex-token 'PROLOGUE start end))))

(define-lex-regex-analyzer wisent-php-lex-epilogue
  "Detect and create an epilogue or percent-percent token."
  "[%?]>"
  (let ((start (match-beginning 0))
        (end   (match-end 0)))
    (semantic-lex-push-token
     (semantic-lex-token 'EPILOGUE start end))))

  
(define-lex-regex-analyzer wisent-php-lex-heredoc
  "Detect and create an epilogue or percent-percent token."
  (concat "<<<[[:blank:]]*\\(" wisent-php-label-regex "\\)$")
  (let ((start (match-beginning 0))
        (end   (progn
		 (re-search-forward (concat "^" (match-string 1) ";") nil t)
		 (match-end 0))))
    (semantic-lex-push-token
     (semantic-lex-token 'STRING_LITERAL start end))
    (setq semantic-lex-end-point end)))

(define-lex-analyzer wisent-php-lex-out-of-php
  "Detect and create python indentation tokens at beginning of line."
  (progn
    (and wisent-in-php
	 (looking-at "[[:space:]\n]*[%?]>")
	 (setq wisent-in-php nil))
    (when (not wisent-in-php)
      (let ((last-pos (point))
	    (token (wisent-php--move-to-php-beginning)))
	(setq semantic-lex-end-point (point))
	(when token
	  (setq wisent-in-php t)))
;;	  (semantic-lex-push-token
;;	   (semantic-lex-token token last-pos (point)))))
      t)))
	
(defun wisent-php-expand-tag (tag)
  "Expand TAG into a list of equivalent tags, or nil.
Expand multiple variable declarations in the same statement, that is
tags of class `variable' whose name is equal to a list of elements of
the form (NAME START . END).  NAME is a variable name.  START and END
are the bounds in the declaration, related to this variable NAME."
  (let (elts elt clone start end xpand)
    (when (and (eq 'variable (semantic-tag-class tag))
               (consp (setq elts (semantic-tag-name tag))))
      ;; There are multiple names in the same variable declaration.
      (while elts
        ;; For each name element, clone the initial tag and give it
        ;; the name of the element.
        (setq elt   (car elts)
              elts  (cdr elts)
              clone (semantic-tag-clone tag (car elt))
              start (if elts  (cadr elt) (semantic-tag-start tag))
              end   (if xpand (cddr elt) (semantic-tag-end   tag))
              xpand (cons clone xpand))
        ;; Set the bounds of the cloned tag with those of the name
        ;; element.
        (semantic-tag-set-bounds clone start end))
      xpand)))

;;;###autoload
(add-hook 'php-mode-hook #'wisent-php-default-setup)

(provide 'wisent-php-tags)

;;; wisent-php-tags.el ends here
