;;; sml-defs.el --- Various definitions for sml-mode

;; Copyright (C) 1999,2000,2003,2005,2007,2012  Stefan Monnier
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:


;;; Code:

(eval-when-compile (require 'cl))

(defgroup sml ()
  "Editing SML code."
  :group 'languages)

(defvar sml-outline-regexp
  ;; `st' and `si' are to match structure and signature.
  "\\|s[ti]\\|[ \t]*\\(let[ \t]+\\)?\\(fun\\|and\\)\\>"
  "Regexp matching a major heading.
This actually can't work without extending `outline-minor-mode' with the
notion of \"the end of an outline\".")

;;; 
;;; Internal defines
;;; 

(defvar sml-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Smarter cursor movement.
    ;; (define-key map [remap forward-sexp] 'sml-user-forward-sexp)
    ;; (define-key map [remap backward-sexp] 'sml-user-backward-sexp)
    ;; Text-formatting commands:
    (define-key map "\C-c\C-m" 'sml-insert-form)
    (define-key map "\C-c\C-i" 'sml-mode-info)
    (define-key map "\M-|" 'sml-electric-pipe)
    (define-key map "\M-\ " 'sml-electric-space)
    (define-key map "\;" 'sml-electric-semi)
    (define-key map "\M-\t" 'sml-back-to-outer-indent)
    ;; Process commands added to sml-mode-map -- these should autoload
    (define-key map "\C-c\C-l" 'sml-load-file)
    (define-key map "\C-c\C-c" 'sml-compile)
    (define-key map "\C-c\C-s" 'switch-to-sml)
    (define-key map "\C-c\C-r" 'sml-send-region)
    (define-key map "\C-c\C-b" 'sml-send-buffer)
    (define-key map [(meta shift down-mouse-1)] 'sml-drag-region)
    map)
  "The keymap used in `sml-mode'.")

(defconst sml-builtin-nested-comments-flag
  (ignore-errors
    (not (equal (let ((st (make-syntax-table)))
		  (modify-syntax-entry ?\* ". 23n" st) st)
		(let ((st (make-syntax-table)))
		  (modify-syntax-entry ?\* ". 23" st) st))))
  "Non-nil means this Emacs understands the `n' in syntax entries.")

(defvar sml-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\* (if sml-builtin-nested-comments-flag
                                 ". 23n" ". 23") st)
    (modify-syntax-entry ?\( "()1" st)
    (modify-syntax-entry ?\) ")(4" st)
    (mapc (lambda (c) (modify-syntax-entry c "_" st)) "._'")
    (mapc (lambda (c) (modify-syntax-entry c "." st)) ",;")
    ;; `!' is not really a prefix-char, oh well!
    (mapc (lambda (c) (modify-syntax-entry c "'"  st)) "~#!")
    (mapc (lambda (c) (modify-syntax-entry c "."  st)) "%&$+-/:<=>?@`^|")
    st)
  "The syntax table used in `sml-mode'.")


(easy-menu-define sml-mode-menu sml-mode-map "Menu used in `sml-mode'."
  '("SML"
    ("Process"
     ["Start default ML compiler" run-sml		t]
     ["-" nil nil]
     ["run CM.make"		sml-compile	t]
     ["load ML source file"	sml-load-file	t]
     ["switch to ML buffer"	switch-to-sml	t]
     ["--" nil nil]
     ["send buffer contents"	sml-send-buffer	t]
     ["send region"		sml-send-region	t]
     ["send paragraph"		sml-send-function t]
     ["goto next error"		next-error	(featurep 'sml-proc)]
     ["---" nil nil]
     ;; ["Standard ML of New Jersey" sml-smlnj	(fboundp 'sml-smlnj)]
     ;; ["Poly/ML"			sml-poly-ml	(fboundp 'sml-poly-ml)]
     ;; ["Moscow ML"		sml-mosml	(fboundp 'sml-mosml)]
     ["Help for Inferior ML"	(describe-function 'inferior-sml-mode) :active (featurep 'sml-proc)])
    ["electric pipe"     sml-electric-pipe t]
    ["insert SML form"   sml-insert-form t]
    ("Forms" :filter sml-forms-menu)
    ("Format/Mode Variables"
     ["indent region"             indent-region t]
     ["outdent"                   sml-back-to-outer-indent t]
     ;; ["-" nil nil]
     ;; ["set indent-level"          sml-indent-level t]
     ;; ["set pipe-indent"           sml-pipe-indent t]
     ;; ["--" nil nil]
     ;; ["toggle type-of-indent"     sml-type-of-indent t]
     ;; ["toggle nested-if-indent"   sml-nested-if-indent t]
     ;; ["toggle electric-semi-mode" sml-electric-semi-mode t]
     )
    ["-----" nil nil]
    ["SML mode help (brief)"       describe-mode t]
    ["SML mode *info*"             sml-mode-info t]
    ["-----" nil nil]
    ["Remove overlay"    (sml-error-overlay 'undo) t ;:active (sml-overlay-active-p)
     ]))

;; Make's sure they appear in the menu bar when sml-mode-map is active.
;; On the hook for XEmacs only -- see easy-menu-add in auc-menu.el.
;; (defun sml-mode-menu-bar ()
;;   "Make sure menus appear in the menu bar as well as under mouse 3."
;;   (and (eq major-mode 'sml-mode)
;;        (easy-menu-add sml-mode-menu sml-mode-map)))
;; (add-hook 'sml-mode-hook 'sml-mode-menu-bar)

;;
;; regexps
;;

(defun sml-syms-re (syms)
  (concat "\\<" (regexp-opt syms t) "\\>"))

;;

(defconst sml-module-head-syms
  '("signature" "structure" "functor" "abstraction"))


(defconst sml-=-starter-syms
  (list* "|" "val" "fun" "and" "datatype" "type" "abstype" "eqtype"
	 sml-module-head-syms)
  "Symbols that can be followed by a `='.")
(defconst sml-=-starter-re
  (concat "\\S.|\\S.\\|" (sml-syms-re (cdr sml-=-starter-syms)))
  "Symbols that can be followed by a `='.")

(defconst sml-non-nested-of-starter-re
  (sml-syms-re '("datatype" "abstype" "exception"))
  "Symbols that can introduce an `of' that shouldn't behave like a paren.")

(defconst sml-starters-syms
  (append sml-module-head-syms
	  '("abstype" "datatype" "exception" "fun"
	    "local" "infix" "infixr" "sharing" "nonfix"
	    "open" "type" "val" "and"
	    "withtype" "with"))
  "The starters of new expressions.")

(defconst sml-pipeheads
   '("|" "of" "fun" "fn" "and" "handle" "datatype" "abstype")
   "A `|' corresponds to one of these.")


(provide 'sml-defs)

;;; sml-defs.el ends here
