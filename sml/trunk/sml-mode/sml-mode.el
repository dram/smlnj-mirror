;;; sml-mode.el. Major mode for editing (Standard) ML. Version 3.3(beta)

(defconst rcsid-sml-mode "@(#)$Name$:$Id$")

;; Copyright (C) 1989-1999, Lars Bo Nielsen; 1994,1997, Matthew J. Morley

;; $Revision$
;; $Date$

;; This file is not part of GNU Emacs, but it is distributed under the
;; same conditions.

;; ====================================================================

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to the
;; Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;; ====================================================================

;;; HISTORY 

;; Still under construction: History obscure, needs a biographer as
;; well as a M-x doctor. Change Log on request.

;; Hacked by Olin Shivers for comint from Lars Bo Nielsen's sml.el.

;; Hacked by Matthew Morley to incorporate Fritz Knabe's hilite and
;; font-lock patterns, some of Steven Gilmore's (reduced) easy-menus,
;; and numerous bugs and bug-fixes.

;; Author: Lars Bo Nielsen
;;      Olin Shivers
;;	Fritz Knabe (?)
;;	Steven Gilmore (?)
;;	Matthew Morley <mjm@scs.leeds.ac.uk> (aka <matthew@verisity.com>)
;;	Matthias Blume <blume@cs.princeton.edu> (aka <blume@kurims.kyoto-u.ac.jp>)
;;      (Stefan Monnier) monnier@cs.yale.edu
;; Maintainer: (Stefan Monnier) monnier+lists/emacs/sml@tequila.cs.yale.edu
;; Keywords: SML

;;; DESCRIPTION 

;; See accompanying info file: sml-mode.info

;;; FOR YOUR .EMACS FILE

;; If sml-mode.el lives in some non-standard directory, you must tell 
;; emacs where to get it. This may or may not be necessary:

;; (setq load-path (cons (expand-file-name "~jones/lib/emacs") load-path))

;; Then to access the commands autoload sml-mode with that command:

;; (autoload 'sml-mode "sml-mode" "Major mode for editing ML programs." t)
;;
;; If files ending in ".sml" or ".ML" are hereafter considered to contain
;; Standard ML source, put their buffers into sml-mode automatically:

;; (setq auto-mode-alist
;;       (cons '(("\\.sml$" . sml-mode)
;;               ("\\.ML$"  . sml-mode)) auto-mode-alist))

;; Here's an example of setting things up in the sml-mode-hook:

;; (setq sml-mode-hook
;;       '(lambda() "ML mode hacks"
;;          (setq sml-indent-level 2         ; conserve on horiz. space
;;                indent-tabs-mode nil)))    ; whatever

;; sml-mode-hook is run whenever a new sml-mode buffer is created.
;; There is an sml-load-hook too, which is only run when this file is
;; loaded. One use for this hook is to select your preferred
;; highlighting scheme, like this:

;; (setq sml-load-hook
;;       '(lambda() "Highlights." (require 'sml-hilite)))

;; hilit19 is the magic that actually does the highlighting. My set up
;; for hilit19 runs something like this:

;; (if window-system
;;     (setq hilit-background-mode   t ; monochrome (alt: 'dark or 'light)
;;           hilit-inhibit-hooks     nil
;;           hilit-inhibit-rebinding nil
;;           hilit-quietly           t))

;; Alternatively, you can (require 'sml-font) which uses the font-lock
;; package instead. 

;; Finally, there are inferior-sml-{mode,load}-hooks -- see comments
;; in sml-proc.el. For much more information consult the mode's *info*
;; tree.

;;; VERSION STRING

(defconst sml-mode-version-string
  "sml-mode, version 3.3")

(require 'cl)
(require 'sml-util)
(require 'sml-move)
(require 'sml-defs)

;;; VARIABLES CONTROLLING INDENTATION

(defvar sml-indent-level 4
  "*Indentation of blocks in ML (see also `sml-structure-indent').")

(defvar sml-structure-indent 4          ; Not currently an option.
  "*Indentation of signature/structure/functor declarations.")

(defvar sml-pipe-indent -2
  "*Extra (usually negative) indentation for lines beginning with `|'.")

(defvar sml-indent-case-arm 0
  "*Indentation of case arms.")

(defvar sml-indent-case-of 2
  "*Indentation of an `of'�on its own line.")

(defvar sml-indent-equal -2
  "*Extra (usually negative) indenting for lines beginning with `='.")

(defvar sml-indent-fn -3
  "*Extra (usually negative) indenting for lines beginning with `fn'.")

;;(defvar sml-indent-paren -1
;;  "*Extra (usually negative) indenting for lines beginning with `('.")

(defvar sml-indent-args 4
  "*Indentation of args placed on a separate line.")

(defvar sml-indent-align-args t
  "*Whether the arguments should be aligned.")

(defvar sml-case-indent nil
  "*How to indent case-of expressions.
    If t:   case expr                     If nil:   case expr of
              of exp1 => ...                            exp1 => ...
               | exp2 => ...                          | exp2 => ...

The first seems to be the standard in SML/NJ, but the second
seems nicer...")

(defvar sml-nested-if-indent nil
  "*Determine how nested if-then-else will be formatted:
    If t: if exp1 then exp2               If nil:   if exp1 then exp2
          else if exp3 then exp4                    else if exp3 then exp4
          else if exp5 then exp6                         else if exp5 then exp6
          else exp7                                           else exp7")

(defvar sml-type-of-indent nil
  "*How to indent `let' `struct' etc.
    If t:  fun foo bar = let              If nil:  fun foo bar = let
                             val p = 4                 val p = 4
                         in                        in
                             bar + p                   bar + p
                         end                       end

Will not have any effect if the starting keyword is first on the line.")

(defvar sml-electric-semi-mode nil
  "*If t, `\;' will self insert, reindent the line, and do a newline.
If nil, just insert a `\;'. (To insert while t, do: C-q \;).")

(defvar sml-paren-lookback 1000
  "*How far back (in chars) the indentation algorithm should look
for open parenthesis. High value means slow indentation algorithm. A
value of 1000 (being the equivalent of 20-30 lines) should suffice
most uses. (A value of nil, means do not look at all)")

;;; OTHER GENERIC MODE VARIABLES

(defvar sml-mode-info "sml-mode"
  "*Where to find Info file for sml-mode.
The default assumes the info file \"sml-mode.info\" is on Emacs' info
directory path. If it is not, either put the file on the standard path
or set the variable sml-mode-info to the exact location of this file
which is part of the sml-mode 3.2 (and later) distribution. E.g:  

  (setq sml-mode-info \"/usr/me/lib/info/sml-mode\") 

in your .emacs file. You can always set it interactively with the
set-variable command.")

(defvar sml-mode-hook nil
  "*This hook is run when sml-mode is loaded, or a new sml-mode buffer created.
This is a good place to put your preferred key bindings.")

(defvar sml-load-hook nil
  "*This hook is run when sml-mode (sml-mode.el) is loaded into Emacs.")

(defvar sml-mode-abbrev-table nil "*SML mode abbrev table (default nil)")

(defvar sml-error-overlay t
  "*Non-nil means use an overlay to highlight errorful code in the buffer.

This gets set when `sml-mode' is invoked\; if you don't like/want SML 
source errors to be highlighted in this way, do something like

  \(setq-default sml-error-overlay nil\)

in your `sml-load-hook', say.")

(make-variable-buffer-local 'sml-error-overlay)

;;; CODE FOR SML-MODE 

(defun sml-mode-info ()
  "Command to access the TeXinfo documentation for sml-mode.
See doc for the variable sml-mode-info."
  (interactive)
  (require 'info)
  (condition-case nil
      (Info-goto-node (concat "(" sml-mode-info ")"))
    (error (progn
             (describe-variable 'sml-mode-info)
             (message "Can't find it... set this variable first!")))))


;;; Autoload functions -- no-doc is another idea cribbed from AucTeX!

(let ((sml-no-doc
       "This function is part of sml-proc, and has not yet been loaded.
Full documentation will be available after autoloading the function."))

  (autoload 'run-sml	   "sml-proc"   sml-no-doc t)
  (autoload 'sml-make	   "sml-proc"   sml-no-doc t)
  (autoload 'sml-load-file   "sml-proc"   sml-no-doc t)

  (autoload 'switch-to-sml   "sml-proc"   sml-no-doc t)
  (autoload 'sml-send-region "sml-proc"   sml-no-doc t)
  (autoload 'sml-send-buffer "sml-proc"   sml-no-doc t)
  (autoload 'sml-next-error  "sml-proc"   sml-no-doc t))

;; font-lock setup

(defconst sml-keywords-regexp
  (sml-syms-re "abstraction" "abstype" "and" "andalso" "as" "before" "case"
	       "datatype" "else" "end" "eqtype" "exception" "do" "fn"
	       "fun" "functor" "handle" "if" "in" "include" "infix"
	       "infixr" "let" "local" "nonfix" "of" "op" "open" "orelse"
	       "overload" "raise" "rec" "sharing" "sig" "signature"
	       "struct" "structure" "then" "type" "val" "where" "while"
	       "with" "withtype")
  "A regexp that matches any and all keywords of SML.")

(defconst sml-font-lock-keywords
  `(;;(sml-font-comments-and-strings)
    ("\\<\\(fun\\|and\\)\\s-+\\(\\sw+\\)"
     (1 font-lock-keyword-face)
     (2 font-lock-function-def-face))
    ("\\<\\(\\(data\\|abs\\|with\\|eq\\)?type\\)\\s-+\\('\\s-*\\sw+\\s-+\\)*\\(\\sw+\\)"
     (1 font-lock-keyword-face)
     (4 font-lock-type-def-face))
    ("\\<\\(val\\)\\s-+\\(\\sw+\\>\\s-*\\)?\\(\\sw+\\)\\s-*="
     (1 font-lock-keyword-face)
     ;;(6 font-lock-variable-def-face nil t)
     (3 font-lock-variable-def-face))
    ("\\<\\(structure\\|functor\\|abstraction\\)\\s-+\\(\\sw+\\)"
     (1 font-lock-keyword-face)
     (2 font-lock-module-def-face))
    ("\\<\\(signature\\)\\s-+\\(\\sw+\\)"
     (1 font-lock-keyword-face)
     (2 font-lock-interface-def-face))
    
    (,sml-keywords-regexp . font-lock-keyword-face))
  "Regexps matching standard SML keywords.")

;; default faces values
(flet ((def-face (face def)
	 "Define a face for font-lock."
	 (unless (boundp face)
	   (set face (cond
		      ((facep face) face)
		      ((facep def) (copy-face def face))
		      (t def))))))
  (def-face 'font-lock-function-def-face 'font-lock-function-name-face)
  (def-face 'font-lock-type-def-face 'font-lock-type-face)
  (def-face 'font-lock-module-def-face 'font-lock-function-name-face)
  (def-face 'font-lock-interface-def-face 'font-lock-type-face)
  (def-face 'font-lock-variable-def-face 'font-lock-variable-name-face))

(defvar sml-syntax-prop-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?l "(d" st)
    (modify-syntax-entry ?s "(d" st)
    (modify-syntax-entry ?d ")l" st)
    (modify-syntax-entry ?* "." st)
    st))

(defun sml-get-depth-st ()
  (save-excursion
    (let* ((disp (if (eq (char-before) ?\)) (progn (backward-char) -1) nil))
	   (foo (backward-char))
	   (disp (if (eq (char-before) ?\() (progn (backward-char) 0) disp))
	   (pt (point)))
      (when disp
	(let* ((depth
		(save-match-data
		  (if (re-search-backward "\\*)\\|(\\*" nil t)
		      (+ (or (get-char-property (point) 'comment-depth) 0)
			 (case (char-after) (?\( 1) (?* 0))
			 disp)
		    0)))
	       (depth (if (> depth 0) depth)))
	  (put-text-property pt (1+ pt) 'comment-depth depth)
	  (when depth sml-syntax-prop-table))))))

(defconst sml-font-lock-syntactic-keywords
  `(;;("\\<\\(l\\)\\(et\\|ocal\\)\\>" (1 ',sml-syntax-prop-table))
    ;;("\\<\\(s\\)\\(ig\\truct\\)\\>" (1 ',sml-syntax-prop-table))
    ;;("\\<en\\(d\\)\\>" (1 ',sml-syntax-prop-table))
    ("(?\\(\\*\\))?" (1 (sml-get-depth-st)))))

(defconst sml-font-lock-defaults
  '(sml-font-lock-keywords nil nil ((?_ . "w") (?' . "w")) nil
			   (font-lock-syntactic-keywords . sml-font-lock-syntactic-keywords)))

;; code to get comment fontification working in the face of recursive
;; comments.  It's lots more work than it should be.	-- stefan
;; (defvar sml-font-cache '((0 . normal))
;;   "List of (POSITION . STATE) pairs for an SML buffer.
;; The STATE is either `normal', `comment', or `string'.  The POSITION is
;; immediately after the token that caused the state change.")
;; (make-variable-buffer-local 'sml-font-cache)

;; (defun sml-font-comments-and-strings (limit)
;;   "Fontify SML comments and strings up to LIMIT.
;; Handles nested comments and SML's escapes for breaking a string over lines.
;; Uses sml-font-cache to maintain the fontification state over the buffer."
;;   (let ((beg (point))
;; 	last class)
;;     (while (< beg limit)
;;       (while (and sml-font-cache
;; 		  (> (caar sml-font-cache) beg))
;; 	(pop sml-font-cache))
;;       (setq last (caar sml-font-cache))
;;       (setq class (cdar sml-font-cache))
;;       (goto-char last)
;;       (cond
;;        ((eq class 'normal)
;; 	(cond
;; 	 ((not (re-search-forward "\\((\\*\\)\\|\\(\"\\)" limit t))
;; 	  (goto-char limit))
;; 	 ((match-beginning 1)
;; 	  (push (cons (point) 'comment) sml-font-cache))
;; 	 ((match-beginning 2)
;; 	  (push (cons (point) 'string) sml-font-cache))))
;;        ((eq class 'comment)
;; 	(cond
;; 	 ((let ((nest 1))
;; 	    (while (and (> nest 0)
;; 			(re-search-forward "\\((\\*\\)\\|\\(\\*)\\)" limit t))
;; 	      (cond
;; 	       ((match-beginning 1) (incf nest))
;; 	       ((match-beginning 2) (decf nest))))
;; 	    (> nest 0))
;; 	  (goto-char limit))
;; 	 (t
;; 	  (push (cons (point) 'normal) sml-font-cache)))
;; 	(put-text-property (- last 2) (point) 'face 'font-lock-comment-face))
;;        ((eq class 'string)
;; 	(while (and (re-search-forward
;; 		     "\\(\"\\)\\|\\(\\\\\\s-*\\\\\\)\\|\\(\\\\\"\\)" limit t)
;; 		     (not (match-beginning 1))))
;; 	(cond
;; 	 ((match-beginning 1)
;; 	  (push (cons (point) 'normal) sml-font-cache))
;; 	 (t
;; 	  (goto-char limit)))
;; 	(put-text-property (- last 1) (point) 'face 'font-lock-string-face)))
;;       (setq beg (point)))))

;;; H A C K   A T T A C K !   X E M A C S   V E R S U S   E M A C S

;; (cond ((fboundp 'make-extent)
;;        ;; suppose this is XEmacs

;;        (defun sml-make-overlay ()
;;          "Create a new text overlay (extent) for the SML buffer."
;;          (let ((ex (make-extent 1 1)))
;;            (set-extent-property ex 'face 'zmacs-region) ex))

;;        (defalias 'sml-is-overlay 'extentp)

;;        (defun sml-overlay-active-p ()
;;          "Determine whether the current buffer's error overlay is visible."
;;          (and (sml-is-overlay sml-error-overlay)
;;               (not (zerop (extent-length sml-error-overlay)))))

;;        (defalias 'sml-move-overlay 'set-extent-endpoints))

;;       ((fboundp 'make-overlay)
       ;; otherwise assume it's Emacs

       (defun sml-make-overlay ()
         "Create a new text overlay (extent) for the SML buffer."
         (let ((ex (make-overlay 0 0)))
           (overlay-put ex 'face 'region) ex))

       (defalias 'sml-is-overlay 'overlayp)

       (defun sml-overlay-active-p ()
         "Determine whether the current buffer's error overlay is visible."
         (and (sml-is-overlay sml-error-overlay)
              (not (equal (overlay-start sml-error-overlay)
                          (overlay-end sml-error-overlay)))))

       (defalias 'sml-move-overlay 'move-overlay);;)
;;       (t
;;        ;; what *is* this!?
;;        (defalias 'sml-is-overlay 'ignore)
;;        (defalias 'sml-overlay-active-p 'ignore)
;;        (defalias 'sml-make-overlay 'ignore)
;;        (defalias 'sml-move-overlay 'ignore)))

;;; MORE CODE FOR SML-MODE

(defun sml-mode-version ()
  "This file's version number (sml-mode)."
  (interactive)
  (message sml-mode-version-string))

;;;###Autoload
(defun sml-mode ()
  "Major mode for editing ML code.
Tab indents for ML code.
Comments are delimited with (* ... *).
Blank lines and form-feeds separate paragraphs.
Delete converts tabs to spaces as it moves back.

For information on running an inferior ML process, see the documentation
for inferior-sml-mode (set this up with \\[sml]).

Customisation: Entry to this mode runs the hooks on sml-mode-hook.

Variables controlling the indentation
=====================================

Seek help (\\[describe-variable]) on individual variables to get current settings.

sml-indent-level (default 4)
    The indentation of a block of code.

sml-pipe-indent (default -2)
    Extra indentation of a line starting with \"|\".

sml-case-indent (default nil)
    Determine the way to indent case-of expression.

sml-nested-if-indent (default nil)
    Determine how nested if-then-else expressions are formatted.

sml-type-of-indent (default nil)
    How to indent let, struct, local, etc.
    Will not have any effect if the starting keyword is first on the line.

sml-electric-semi-mode (default nil)
    If t, a `\;' will reindent line, and perform a newline.

sml-paren-lookback (default 1000)
    Determines how far back (in chars) the indentation algorithm should 
    look to match parenthesis. A value of nil, means do not look at all.

Mode map
========
\\{sml-mode-map}"

  (interactive)
  (kill-all-local-variables)
  (sml-mode-variables)
  (use-local-map sml-mode-map)
  (setq major-mode 'sml-mode)
  (setq mode-name "SML")
  (set (make-local-variable 'outline-regexp) sml-outline-regexp)
  (run-hooks 'sml-mode-hook))            ; Run the hook last

(defun sml-mode-variables ()
  (set-syntax-table sml-mode-syntax-table)
  (setq local-abbrev-table sml-mode-abbrev-table)
  ;; A paragraph is separated by blank lines or ^L only.
  
  (set (make-local-variable 'paragraph-start)
       (concat "^[\t ]*$\\|" page-delimiter))
  (set (make-local-variable 'paragraph-separate) paragraph-start)
  (set (make-local-variable 'indent-line-function) 'sml-indent-line)
  (set (make-local-variable 'comment-start) "(* ")
  (set (make-local-variable 'comment-end) " *)")
  (set (make-local-variable 'comment-column) 40)
  (set (make-local-variable 'comment-start-skip) "(\\*+[ \t]?")
  (set (make-local-variable 'comment-indent-function) 'sml-comment-indent)
  (set (make-local-variable 'font-lock-defaults) sml-font-lock-defaults)
  ;;(set (make-local-variable 'parse-sexp-lookup-properties) t)
  ;;(set (make-local-variable 'parse-sexp-ignore-comments) t)
  (setq sml-error-overlay (and sml-error-overlay (sml-make-overlay))))

(defun sml-error-overlay (undo &optional beg end buffer)
  "Move `sml-error-overlay' so it surrounds the text region in the
current buffer. If the buffer-local variable `sml-error-overlay' is
non-nil it should be an overlay \(or extent, in XEmacs speak\)\; this
function moves the overlay over the current region. If the optional
BUFFER argument is given, move the overlay in that buffer instead of
the current buffer.

Called interactively, the optional prefix argument UNDO indicates that
the overlay should simply be removed: \\[universal-argument] \
\\[sml-error-overlay]."
  (interactive "P")
  (save-excursion
    (set-buffer (or buffer (current-buffer)))
    (if (sml-is-overlay sml-error-overlay)
        (if undo
            (sml-move-overlay sml-error-overlay 1 1)
          ;; if active regions, signals mark not active if no region set
          (let ((beg (or beg (region-beginning)))
                (end (or end (region-end))))
            (sml-move-overlay sml-error-overlay beg end))))))

(defun sml-electric-pipe ()
  "Insert a \"|\".
Depending on the context insert the name of function, a \"=>\" etc."
  (interactive)
  (sml-with-ist
   (let ((text
	  (save-excursion
	    (sml-find-matching-starter sml-pipehead-re)
	    (cond
	     ;; It was a function, insert the function name
	     ((or (looking-at "fun\\>")
		  (and (looking-at "and\\>")
		       (save-excursion
			 (sml-find-matching-starter
			  (sml-syms-re "datatype" "abstype" "fun"))
			 (looking-at "fun\\>"))))
	      (forward-word 1) (sml-forward-spaces)
	      (concat
	       (buffer-substring (point) (progn (forward-word 1) (point)))
	       "  = "))

	     ((looking-at (sml-syms-re "case" "handle" "fn")) " => ")
	     ((looking-at (sml-syms-re "abstype" "datatype" "and")) "")
	     (t (error "Wow, now, there's a bug"))))))

     (unless (save-excursion (skip-chars-backward "\t ") (bolp)) (insert "\n"))
     (insert "| " text)
     (sml-indent-line)
     (beginning-of-line)
     (skip-chars-forward "\t |")
     (skip-syntax-forward "w")
     (skip-chars-forward "\t ")
     (when (= ?= (char-after)) (backward-char)))))

(defun sml-electric-semi ()
  "Inserts a \;.
If variable sml-electric-semi-mode is t, indent the current line, insert 
a newline, and indent."
  (interactive)
  (insert "\;")
  (if sml-electric-semi-mode
      (reindent-then-newline-and-indent)))

;;; INDENTATION !!!

(defun sml-mark-function ()
  "Synonym for mark-paragraph -- sorry.
If anyone has a good algorithm for this..."
  (interactive)
  (mark-paragraph))

(defun sml-indent-region (begin end)
  "Indent region of ML code."
  (interactive "r")
  (message "Indenting region...")
  (save-excursion
    (goto-char end) (setq end (point-marker)) (goto-char begin)
    (while (< (point) end)
      (skip-chars-forward "\t\n ")
      (sml-indent-line)
      (end-of-line))
    (move-marker end nil))
  (message "Indenting region... done"))

(defun sml-indent-line ()
  "Indent current line of ML code."
  (interactive)
  (let ((indent (sml-calculate-indentation)))
    (if (/= (current-indentation) indent)
        (save-excursion                 ;; Added 890601 (point now stays)
          (let ((beg (progn (beginning-of-line) (point))))
            (skip-chars-forward "\t ")
            (delete-region beg (point))
            (indent-to indent))))
    ;; If point is before indentation, move point to indentation
    (if (< (current-column) (current-indentation))
        (skip-chars-forward "\t "))))

(defun sml-back-to-outer-indent ()
  "Unindents to the next outer level of indentation."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward "\t ")
    (let ((start-column (current-column))
          (indent (current-column)))
      (if (> start-column 0)
          (progn
            (save-excursion
              (while (>= indent start-column)
                (if (re-search-backward "^[^\n]" nil t)
                    (setq indent (current-indentation))
                  (setq indent 0))))
            (backward-delete-char-untabify (- start-column indent)))))))

(defun sml-find-comment-indent ()
  (save-excursion
    (let ((depth 1))
      (while (> depth 0)
	(if (re-search-backward "(\\*\\|\\*)" nil t)
	    (cond
	     ((looking-at "*)") (incf depth))
	     ((looking-at comment-start-skip) (decf depth)))
	  (setq depth -1)))
      (if (= depth 0)
	  (current-column)
	nil))))

(defun sml-calculate-indentation ()
  (save-excursion
    (beginning-of-line) (skip-chars-forward "\t ")
    (sml-with-ist
     (let ((indent 0)
	   (sml-point (point)))
       (or
	;;(and (bobp) 0)

	;; Indentation for comments alone on a line, matches the
	;; proper indentation of the next line.
	(and (looking-at comment-start-skip) (sml-forward-spaces) nil)

	;; continued comment
	(and (looking-at "\\*") (setq indent (sml-find-comment-indent))
	     (1+ indent))

	;; Continued string ? (Added 890113 lbn)
	(and (looking-at "\\\\")
	     (save-excursion
	       (if (save-excursion (previous-line 1)
				   (beginning-of-line)
				   (looking-at "[\t ]*\\\\"))
		   (progn (previous-line 1) (current-indentation))
		 (if (re-search-backward "[^\\\\]\"" nil t)
		     (1+ (current-indentation))
		   0))))

	(and (looking-at "in\\>")	; Match the beginning let/local
	     (sml-find-match-indent "\\<in\\>" "\\<l\\(ocal\\|et\\)\\>"))

	(and (looking-at "end\\>")	; Match the beginning
	     ;; FIXME: should match "in" if available.  Or maybe not
	     (sml-find-match-indent "\\<end\\>" sml-begin-symbols-re))

	(and (looking-at "else\\>")	; Match the if
	     (progn
	       (sml-find-match-backward "\\<else\\>" "\\<if\\>")
	       (sml-move-if (backward-word 1)
			    (and sml-nested-if-indent
				 (looking-at "else[ \t]+if\\>")))
	       (current-column)))

	(and (looking-at "then\\>")	; Match the if + extra indentation
	     (sml-find-match-indent "\\<then\\>" "\\<if\\>" t))

	(and (looking-at "of\\>")
	     (progn
	       (sml-find-match-backward "\\<of\\>" "\\<case\\>")
	       (+ (current-column) sml-indent-case-of)))

	(and (looking-at sml-starters-re)
	     (let ((sym (sml-move-read (sml-move-if (not (sml-backward-arg))))))
	       (if sym (sml-get-sym-indent sym)
		 (sml-find-matching-starter sml-starters-re)
		 (current-column))))

	(and (looking-at "|") (sml-indent-pipe))

	(sml-indent-arg)
	(sml-indent-default))))))

;; 	  (let ((indent (current-column)))
;; 	    ;;(skip-chars-forward "\t (")
;; 	    (cond
;; 	     ;; a "let fun" or "let val"
;; 	     ((looking-at "let \\(fun\\|val\\)\\>")
;; 	      (+ (current-column) 4 sml-indent-level))
;; 	     ;; Started val/fun/structure...
;; 	     ;; Indent after "=>" pattern, but only if its not an fn _ =>
;; 	     ;; (890726)
;; 	     ((looking-at ".*=>")
;; 	      (if (looking-at ".*\\<fn\\>.*=>")
;; 		  indent
;; 		(+ indent sml-indent-case-arm)))
;; 	     ;; else keep the same indentation as previous line
;; 	     (t indent)))))))))


	;;(and (setq indent (sml-get-indent)) nil)

	;;(and (looking-at "=[^>]") (+ indent sml-indent-equal))
	;;(and (looking-at "fn\\>") (+ indent sml-indent-fn))
	;;       (and (looking-at "(") (+ indent sml-indent-paren))

	;;(and sml-paren-lookback    ; Look for open parenthesis ?
	;;    (max indent (sml-get-paren-indent)))
	;;indent)))))

(defun sml-indent-pipe ()
  (when (sml-find-matching-starter (concat "|\\|\\<of\\>\\|" sml-pipehead-re)
				   (sml-op-prec "|" 'back))
    (if (looking-at "|")
	(if (sml-bolp) (current-column) (sml-indent-pipe))
      (cond
       ((looking-at "datatype")
	(re-search-forward "=")
	(forward-char))
       ((looking-at "case\\>")
	(sml-forward-sym)	;skip `case'
	(sml-find-match-forward "\\<case\\>" "\\<of\\>"))
       (t
	(forward-word 1)))
      (sml-forward-spaces)
      (+ sml-pipe-indent (current-column)))))


(defun sml-indent-arg ()
  (and (save-excursion (ignore-errors (sml-forward-arg)))
       ;;(not (looking-at sml-not-arg-re))
       ;; looks like a function or an argument
       (sml-move-if (sml-backward-arg))
       ;; an argument
       (if (save-excursion (not (sml-backward-arg)))
	   ;; a first argument
	   (+ (current-column) sml-indent-args)
	 ;; not a first arg
	 (while (and (/= (current-column) (current-indentation))
		     (sml-move-if (sml-backward-arg))))
	 (unless (save-excursion (sml-backward-arg))
	   ;; all earlier args are on the same line
	   (sml-forward-arg) (sml-forward-spaces))
	 (current-column))))

(defun sml-re-assoc (al sym)
  (when sym
    (cdr (assoc* sym al
		 :test (lambda (x y) (string-match y x))))))
(defun sml-get-indent (data n &optional strict)
  (eval (if (listp data)
	    (nth n data)
	  (and (not strict) data))))

(defun sml-dangling-sym ()
  (save-excursion
    (and (not (sml-bolp))
	 (< (sml-point-after (end-of-line))
	    (sml-point-after (sml-forward-sym)
			     (sml-forward-spaces))))))

(defun sml-get-sym-indent (sym &optional style)
  "expects to be looking-at SYM."
  (let ((indent-data (sml-re-assoc sml-indent-starters sym))
	(delegate (eval (sml-re-assoc sml-delegate sym))))
    (or (when indent-data
	  (if (or style (not delegate))
	      ;; normal indentation
	      (let ((indent (sml-get-indent indent-data (or style 0))))
		(when indent
		  (+ (if (sml-dangling-sym)
			 (sml-indent-default 'noindent)
		       (current-column))
		     indent)))
	    ;; delgate indentation to the parent
	    (sml-forward-sym) (sml-backward-sexp nil)
	    (let* ((parent-sym (save-excursion (sml-move-read (sml-forward-sym))))
		   (parent-indent (sml-re-assoc sml-indent-starters parent-sym)))
	      ;; check the special rules
	      (sml-move-if (backward-word 1)
			   (looking-at "\\<else[ \t]+if\\>"))
	      (+ (if (sml-dangling-sym)
		     (sml-indent-default 'noindent)
		   (current-column))
		 (or (sml-get-indent indent-data 1 'strict)
		     (sml-get-indent parent-indent 1 'strict)
		     (sml-get-indent indent-data 0)
		     (sml-get-indent parent-indent 0))))))
	;; (save-excursion
	;;   (sml-forward-sym)
	;;   (when (> (sml-point-after (end-of-line))
	;; 	      (progn (sml-forward-spaces) (point)))
	;;     (current-column)))
	)))

(defun sml-indent-default (&optional noindent)
  (let* ((sym-after (save-excursion (sml-move-read (sml-forward-sym))))
	 (prec-after (sml-op-prec sym-after 'back))
	 (_ (sml-backward-spaces))
	 (sym-before (sml-move-read (sml-backward-sym)))
	 (prec (or (sml-op-prec sym-before 'back) prec-after 100))
	 sexp)
    (or (and sym-before (sml-get-sym-indent sym-before))
	(progn
	  ;;(sml-forward-sym)
	  (while (and (not (sml-bolp))
		      (sml-move-if (sml-backward-sexp (1- prec)))
		      (not (sml-bolp)))
	    (while (sml-move-if (sml-backward-sexp prec))))
	  (or (and (not (sml-bolp))
		   (= prec 65) (string-equal "=" sym-before) ;Yuck!!
	           (save-excursion
		     (sml-backward-spaces)
		     (let* ((sym (sml-move-read (sml-backward-sym)))
			    (sym-indent (sml-re-assoc sml-indent-starters sym)))
		       (when sym-indent
			 (if noindent
			     (current-column)
			   (sml-get-sym-indent sym 1))))))
	      (current-column))))))


(defun sml-bolp ()
  (save-excursion
    (skip-chars-backward " \t|") (bolp)))

;; (defun sml-goto-first-subexp ()
;;   (let ((initpoint (point)))
    
;;     (let ((argp (and (looking-at "[[({a-zA-Z0-9_'#~]\\|$")
;; 		     (not (looking-at (concat "[ \t]*" sml-not-arg-regexp))))))
;;       (while (and argp (not (bobp)))
;; 	(let* ((endpoint (point))
;; 	       (startpoint endpoint))
;; 	  (setq argp
;; 		(ignore-errors
;; 		 (sml-backward-sexp t)
;; 		 (setq startpoint (point))
;; 		 (and (not (looking-at (concat "[[(]\\|" sml-keywords-regexp)))
;; 		      (progn (sml-forward-sexp)
;; 			     (sml-skip-spaces)
;; 			     (>= (point) endpoint)))))
;; 	  (goto-char (if argp startpoint endpoint))))
;;       (let ((res (point)))
;; 	(sml-backward-spaces) (skip-syntax-backward "^ ")
;; 	(if (looking-at "*\\|:[^=]\\|->\\|of\\>")
;; 	    (goto-char initpoint)
;; 	  (goto-char res)
;; 	  (sml-skip-spaces))))))

;; maybe `|' should be set to word-syntax in our temp syntax table ?
(defun sml-current-indentation ()
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward " \t|")
    (current-column)))

;; (defun sml-get-indent ()
;;   (save-excursion
;;     ;;(let ((endpoint (point)))

;;       ;; let's try to see whether we are inside an `f a1 a2 ..' expression
;;       ;;(sml-goto-first-subexp)
;;       ;;(setq rover (current-column))
;;       ;;(sml-skip-spaces)
;;       (cond
;; ;;        ((< (point) endpoint)
;; ;; 	;; we're not the first subexp
;; ;; 	(sml-forward-sexp)
;; ;; 	(if (and sml-indent-align-args
;; ;; 		 (progn (sml-skip-spaces) (< (point) endpoint)))
;; ;; 	    ;; we're not the second subexp
;; ;; 	    (current-column)
;; ;; 	  (+ rover sml-indent-args)))

;;        ;; we're not inside an `f a1 a2 ..' expr
;;        ((progn ;;(goto-char endpoint)
;; 	       (sml-backward-spaces)
;; 	       (/= (skip-chars-backward ";,") 0))
;; 	(sml-backward-sexps (concat "[[(]\\'\\|" sml-user-begin-symbols-re))
;; 	(current-column))

;;        (t
;; 	(while (/= (current-column) (current-indentation))
;; 	  (sml-backward-sexp t))
;; 	(when (looking-at "\\<of\\>") (forward-word 1))
;; 	(skip-chars-forward "\t |")
;; 	(let ((indent (current-column)))
;; 	  ;;(skip-chars-forward "\t (")
;; 	  (cond
;; 	   ;; a "let fun" or "let val"
;; 	   ((looking-at "let \\(fun\\|val\\)\\>")
;; 	    (+ (current-column) 4 sml-indent-level))
;; 	   ;; Started val/fun/structure...
;; 	   ((looking-at sml-indent-starters-reg)
;; 	    (+ (current-column) sml-indent-level))
;; 	   ;; Indent after "=>" pattern, but only if its not an fn _ =>
;; 	   ;; (890726)
;; 	   ((looking-at ".*=>")
;; 	    (if (looking-at ".*\\<fn\\>.*=>")
;; 		indent
;; 	      (+ indent sml-indent-case-arm)))
;; 	   ;; else keep the same indentation as previous line
;; 	   (t indent)))))))

;; (defun sml-get-paren-indent ()
;;   (save-excursion
;;     (condition-case ()
;; 	(progn
;; 	  (up-list -1)
;; 	  (if (save-excursion
;; 		(forward-char 1)
;; 		(looking-at sml-indent-starters-reg))
;; 	      (1+ (+ (current-column) sml-indent-level))
;; 	    (1+ (current-column))))
;;       (error 0))))

;; (defun sml-inside-comment-or-string-p ()
;;   (let ((start (point)))
;;     (if (save-excursion
;;           (condition-case ()
;;               (progn
;;                 (search-backward "(*")
;;                 (search-forward "*)")
;;                 (forward-char -1)       ; A "*)" is not inside the comment
;;                 (> (point) start))
;;             (error nil)))
;;         t
;;       (let ((numb 0))
;;         (save-excursion
;;           (save-restriction
;;             (narrow-to-region (progn (beginning-of-line) (point)) start)
;;             (condition-case ()
;;                 (while t
;;                   (search-forward "\"")
;;                   (setq numb (1+ numb)))
;;               (error (if (and (not (zerop numb))
;;                               (not (zerop (% numb 2))))
;;                          t nil)))))))))

;; (defun sml-find-match-backward (unquoted-this this match)
;;   (let ((case-fold-search nil)
;; 	(level 1)
;; 	(pattern (concat this "\\|" match)))
;;     (while (not (zerop level))
;;       (if (sml-re-search-backward pattern)
;; 	  (setq level (cond
;; 		       ((looking-at this) (1+ level))
;; 		       ((looking-at match) (1- level))))
;; 	;; The right match couldn't be found
;; 	(error (concat "Unbalanced: " unquoted-this))))))

(defun sml-find-match-indent (this match &optional indented)
  (save-excursion
    (sml-find-match-backward this match)
    (if (or indented (not (sml-dangling-sym)))
        (current-column)
      (sml-indent-default 'noindent))))

(defun sml-find-matching-starter (regexp &optional prec)
  (sml-backward-sexp prec)
  (while (not (or (looking-at regexp) (bobp)))
    (sml-backward-sexp prec))
  (not (bobp)))

;; (defun sml-re-search-backward (regexpr)
;;   (let ((case-fold-search nil) (found t))
;;     (if (re-search-backward regexpr nil t)
;;         (progn
;;           (condition-case ()
;;               (while (sml-inside-comment-or-string-p)
;;                 (re-search-backward regexpr))
;;             (error (setq found nil)))
;;           found)
;;       nil)))

(defun sml-comment-indent ()
  (if (looking-at "^(\\*")              ; Existing comment at beginning
      0                                 ; of line stays there.
    (save-excursion
      (skip-chars-backward " \t")
      (max (1+ (current-column))        ; Else indent at comment column
           comment-column))))           ; except leave at least one space.

;;; INSERTING PROFORMAS (COMMON SML-FORMS) 

(defvar sml-forms-alist
  '(("let") ("local") ("case") ("abstype") ("datatype")
    ("signature") ("structure") ("functor"))
  "*The list of templates to auto-insert.

You can extend this alist to your heart's content. For each additional
template NAME in the list, declare a keyboard macro or function (or
interactive command) called 'sml-form-NAME'.

If 'sml-form-NAME' is a function it takes no arguments and should
insert the template at point\; if this is a command it may accept any
sensible interactive call arguments\; keyboard macros can't take
arguments at all. Apropos keyboard macros, see `name-last-kbd-macro'
and `sml-addto-forms-alist'.

`sml-forms-alist' understands let, local, case, abstype, datatype,
signature, structure, and functor by default.")

;; See also macros.el in emacs lisp dir.

(defun sml-addto-forms-alist (name)
  "Assign a name to the last keyboard macro defined.
Argument NAME is transmogrified to sml-form-NAME which is the symbol
actually defined. 

The symbol's function definition becomes the keyboard macro string.

If that works, NAME is added to `sml-forms-alist' so you'll be able to
reinvoke the macro through \\[sml-insert-form]. You might want to save
the macro to use in a later editing session -- see `insert-kbd-macro'
and add these macros to your .emacs file.

See also `edit-kbd-macro' which is bound to \\[edit-kbd-macro]."
  (interactive "sName for last kbd macro (\"sml-form-\" will be added): ")
  (if (string-equal name "")
      (error "No command name given")
    (name-last-kbd-macro (intern (concat "sml-form-" name)))
    (message (concat "Macro bound to sml-form-" name))
    (or (assoc name sml-forms-alist)
        (setq sml-forms-alist (cons (list name) sml-forms-alist)))))

;; at a pinch these could be added to SML/Forms menu through the good
;; offices of activate-menubar-hook or something... but documentation
;; of this and/or menu-bar-update-hook is sparse in 19.33. anyway, use
;; completing read for sml-insert-form prompt...

(defvar sml-last-form "let"
  "The most recent sml form inserted.")

(defun sml-insert-form (arg)
  "Interactive short-cut to insert a common ML form.
If a perfix argument is given insert a newline and indent first, or
just move to the proper indentation if the line is blank\; otherwise
insert at point (which forces indentation to current column).

The default form to insert is 'whatever you inserted last time'
\(just hit return when prompted\)\; otherwise the command reads with 
completion from `sml-forms-alist'."
  (interactive "P")
  (let ((name (completing-read
               (format "Form to insert: (default %s) " sml-last-form)
               sml-forms-alist nil t nil)))
    ;; default is whatever the last insert was...
    (if (string= name "") (setq name sml-last-form))
    (setq sml-last-form name)
    (if arg
        (if (save-excursion (beginning-of-line) (looking-at "[ \t]*$"))
            (sml-indent-line)
          (newline-and-indent)))
    (cond ((string= name "let") (sml-form-let))
          ((string= name "local") (sml-form-local))
          ((string= name "case") (sml-form-case))
          ((string= name "abstype") (sml-form-abstype))
          ((string= name "datatype") (sml-form-datatype))
          ((string= name "functor") (sml-form-functor))
          ((string= name "structure") (sml-form-structure))
          ((string= name "signature") (sml-form-signature))
          (t
           (let ((template (intern (concat "sml-form-" name))))
             (if (fboundp template)
                 (if (commandp template)
                     ;; it may be a named kbd macro too
                     (command-execute template)
                   (funcall template))
               (error
                (format "Undefined format function: %s" template))))))))

(defun sml-form-let () 
  "Insert a `let in end' template."
  (interactive)
  (sml-let-local "let"))

(defun sml-form-local ()
  "Insert a `local in end' template."
  (interactive)
  (sml-let-local "local"))

(defun sml-let-local (starter)
  "Insert a let or local template, depending on STARTER string."
  (let ((indent (current-column)))
    (insert starter)
    (insert "\n") (indent-to (+ sml-indent-level indent))
    (save-excursion                     ; so point returns here
      (insert "\n")
      (indent-to indent)
      (insert "in\n")
      (indent-to (+ sml-indent-level indent))
      (insert "\n")
      (indent-to indent)
      (insert "end"))))

(defun sml-form-case ()
  "Insert a case expression template, prompting for the case-expresion."
  (interactive)
  (let ((expr (read-string "Case expr: "))
        (indent (current-column)))
    (insert (concat "case " expr))
    (if sml-case-indent
        (progn
          (insert "\n")
          (indent-to (+ 2 indent))
          (insert "of "))
      (insert " of\n")
      (indent-to (+ indent sml-indent-level)))
    (save-excursion (insert " => "))))

(defun sml-form-signature ()
  "Insert a generative signature binding, prompting for the name."
  (interactive)
  (let ((indent (current-column))
        (name (read-string "Signature name: ")))
    (insert (concat "signature " name " ="))
    (insert "\n")
    (indent-to (+ sml-structure-indent indent))
    (insert "sig\n")
    (indent-to (+ sml-structure-indent sml-indent-level indent))
    (save-excursion
      (insert "\n")
      (indent-to (+ sml-structure-indent indent))
      (insert "end"))))

(defun sml-form-structure ()
  "Insert a generative structure binding, prompting for the name.
The command also prompts for any signature constraint -- you should
specify \":\" or \":>\" and the constraining signature."
  (interactive)
  (let ((indent (current-column))
        (name (read-string (concat "Structure name: ")))
        (signame (read-string "Signature constraint (default none): ")))
    (insert (concat "structure " name " "))
    (insert (if (string= "" signame) "=" (concat signame " =")))
    (insert "\n")
    (indent-to (+ sml-structure-indent indent))
    (insert "struct\n")
    (indent-to (+ sml-structure-indent sml-indent-level indent))
    (save-excursion
      (insert "\n")
      (indent-to (+ sml-structure-indent indent))
      (insert "end"))))

(defun sml-form-functor ()
  "Insert a genarative functor binding, prompting for the name.
The command also prompts for the required signature constraint -- you
should specify \":\" or \":>\" and the constraining signature."
  (interactive)
  (let ((indent(current-indentation))
        (name (read-string "Name of functor: "))
        (signame (read-string "Signature constraint: " ":" )))
    (insert (concat "functor " name " () " signame " ="))
    (insert "\n")
    (indent-to (+ sml-structure-indent indent))
    (insert "struct\n")
    (indent-to (+ sml-structure-indent sml-indent-level indent))
    (save-excursion                     ; return to () instead?
      (insert "\n")
      (indent-to (+ sml-structure-indent indent))
      (insert "end"))))

(defun sml-form-datatype ()
  "Insert a datatype declaration, prompting for name and type parameter."
  (interactive)
  (let ((indent (current-indentation))
        (type (read-string "Datatype type parameter (default none): "))
        (name (read-string (concat "Name of datatype: "))))
    (insert (concat "datatype "
                    (if (string= type "") "" (concat type " "))
                    name " ="))
    (insert "\n")
    (indent-to (+ sml-indent-level indent))))

(defun sml-form-abstype ()
  "Insert an abstype declaration, prompting for name and type parameter."
  (interactive)
  (let ((indent(current-indentation))
        (type (read-string "Abstype type parameter (default none): "))
        (name (read-string "Name of abstype: ")))
    (insert (concat "abstype "
                    (if (string= type "") "" (concat type " "))
                    name " ="))
    (insert "\n")
    (indent-to (+ sml-indent-level indent))
    (save-excursion
      (insert "\n")
      (indent-to indent)
      (insert "with\n")
      (indent-to (+ sml-indent-level indent))
      (insert "\n")
      (indent-to indent)
      (insert "end"))))

;;; Load the menus, if they can be found on the load-path

(condition-case nil
    (require 'sml-menus)
  (error (message "Sorry, not able to load SML mode menus.")))

;;; & do the user's customisation

(add-hook 'sml-load-hook 'sml-mode-version t)

(run-hooks 'sml-load-hook)

;;; sml-mode.el has just finished.
(provide 'sml-mode)
