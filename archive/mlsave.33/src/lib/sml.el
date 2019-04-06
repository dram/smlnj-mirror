;;; This is a primitive gnu emacs sml mode originally contributed by Mitch Wand
;;; and slightly modifiled by Dave MacQueen (dbm).  If you have a better mode
;;; or make improvements, I would appreciate hearing from you.
;;;   --- Dave MacQueen (macqueen@research.att.com)

;;; Here it is, for what it's worth.  As the recent posting on unix-emacs said,
;;; getting an inferior SML process going is easy; what's needed is an intelligent
;;; indenter...   --Mitch  (wand@corwin.ccs.northeastern.edu)

;;; Simple mode for dealing with Standard ML files

;;; Initially a query/replace of scheme.el, with no indenting

;;; Added back-to-outer-indent to unindent a level and bound it to M-tab (dbm)

(message "~wand/emacs/sml: loading")

(autoload 'make-shell "shell")
(defvar sml-mode-syntax-table nil "")
(defvar sml-mode-abbrev-table nil "")

(if t ; (not sml-mode-is-loaded)
    (let ((i 0))
      (setq sml-mode-syntax-table (make-syntax-table))
      (set-syntax-table sml-mode-syntax-table)
      (while (< i ?0)
	(modify-syntax-entry i "_   ")
	(setq i (1+ i)))
      (setq i (1+ ?9))
      (while (< i ?A)
	(modify-syntax-entry i "_   ")
	(setq i (1+ i)))
      (setq i (1+ ?Z))
      (while (< i ?a)
	(modify-syntax-entry i "_   ")
	(setq i (1+ i)))
      (setq i (1+ ?z))
      (while (< i 128)
	(modify-syntax-entry i "_   ")
	(setq i (1+ i)))
      (modify-syntax-entry ?  "    ")
      (modify-syntax-entry ?\t "    ")
      (modify-syntax-entry ?\n ">   ")
      (modify-syntax-entry ?\f ">   ")
      (modify-syntax-entry ?\; "<   ")
      (modify-syntax-entry ?` "'   ")
      (modify-syntax-entry ?' "'   ")
      (modify-syntax-entry ?, "'   ")
      (modify-syntax-entry ?. "'   ")
      (modify-syntax-entry ?# "'   ")
      (modify-syntax-entry ?\" "\"    ")
      (modify-syntax-entry ?\\ "\\   ")
      (modify-syntax-entry ?\[ "(]  ")
      (modify-syntax-entry ?\] ")[  ")
      (modify-syntax-entry ?\{ "(}  ")	; make {} a pair of parens
      (modify-syntax-entry ?\} "){  ")
      (modify-syntax-entry ?\( "()  ")
      (modify-syntax-entry ?\) ")(  ")))


(define-abbrev-table 'sml-mode-abbrev-table ())

(defun sml-mode-variables ()
  (set-syntax-table sml-mode-syntax-table)
  (setq local-abbrev-table sml-mode-abbrev-table)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "^$\\|" page-delimiter))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'indent-relative) ; indent as in indented text mode
  (make-local-variable 'comment-start)	
  (setq comment-start "(\* ")
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "(\\* *")
  (make-local-variable 'comment-end)
  (setq comment-end " \*)")
  (make-local-variable 'comment-multi-line)
  (setq comment-multi-line t)
  (make-local-variable 'comment-column)	
  (setq comment-column 40)
  (make-local-variable 'comment-indent-hook) ; I'm not sure about this one
  (setq comment-indent-hook 'sml-comment-indent)
  )

(defun sml-comment-indent () 0)

(defvar sml-mode-map nil "")

(if t ;(not sml-mode-is-loaded)
    (progn (setq sml-mode-map (make-sparse-keymap))
	   (define-key sml-mode-map "\eo" 'sml-send-buffer)
	   (define-key sml-mode-map "\ez" 'sml-send-defun)
;	   (define-key sml-mode-map "\e\C-q" 'sml-indent-sexp)
;	   (define-key sml-mode-map "\eq" 'sml-indent-sexp)
;	   (define-key sml-mode-map "\e\C-s" 'find-sml-definition)
	   (define-key sml-mode-map "\e\C-x" 'sml-send-defun-and-go)
;	   (define-key sml-mode-map "\e\C-z" 'resume-sml)
	   (define-key sml-mode-map "\177"  ; experimental deletion
	     'backward-delete-char-untabify)
;	   (define-key sml-mode-map "\t" 'sml-indent-line)
	   (define-key sml-mode-map "\e\t" 'back-to-outer-indent)
	   ))

(defun sml-mode ()
  "Major mode for editing Sml code.
Commands:
Delete converts tabs to spaces as it moves back.
\\{sml-mode-map}
Blank lines separate paragraphs.  Comments start with (* , end with *)
Defuns don't work; sml-send-defun sends current paragraph.

Entry to this mode calls the value of sml-mode-hook
if that value is non-nil."
  (interactive)
  (kill-all-local-variables)
  (use-local-map sml-mode-map)
  (setq major-mode 'sml-mode)
  (setq mode-name "Standard ML")
  (sml-mode-variables)
  (and (boundp 'sml-mode-hook)
       sml-mode-hook
       (funcall sml-mode-hook)))

(defvar sml-command "sml"
  "*command executed by run-sml")

(defvar inferior-sml-mode-map nil)

(defun inferior-sml-mode ()
  "Major mode for interacting with an inferior Sml process.

Commands:
Delete converts tabs to spaces as it moves back.
Paragraphs are separated only by blank lines.  

Return at end of buffer sends line as input.
Return not at end copies rest of line to end and sends it.
[The following may not be accurate:]
C-d at end of buffer sends end-of-file as input.
C-d not at end or with arg deletes or kills characters.
C-u and C-w are kill commands, imitating normal Unix input editing.
C-c interrupts the shell or its current subjob if any.
C-z stops, likewise.  C-\\ sends quit signal, likewise.

C-x C-k deletes last batch of output from shell.
C-x C-v puts top of last batch of output at top of window.

Entry to this mode calls the value of sml-mode-hook with no arguments,
if that value is non-nil.  Likewise with the value of shell-mode-hook.
sml-mode-hook is called after shell-mode-hook.

You can send text to the inferior Sml from other buffers
using the commands \\[send-region], \\[send-string] and \\[sml-send-defun]."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'inferior-sml-mode)
  (setq mode-name "Inferior SML")
  (setq mode-line-format 
	"--%1*%1*-%17b   %M   %[(%m: %s)%]----%3p--%-")
  (or inferior-sml-mode-map
      (progn
	(setq inferior-sml-mode-map (copy-sequence shell-mode-map))
	(define-key inferior-sml-mode-map "\e\C-x" 'sml-send-defun)))
  (sml-mode-variables)        
  (use-local-map inferior-sml-mode-map)
  (make-local-variable 'last-input-start)
  (setq last-input-start (make-marker))
  (make-local-variable 'last-input-end)
  (setq last-input-end (make-marker))
  (and (boundp 'shell-mode-hook)
       shell-mode-hook
       (funcall shell-mode-hook))
  (and (boundp 'sml-mode-hook)
       sml-mode-hook
       (funcall sml-mode-hook)))

(defun run-sml ()
  "Run an inferior Sml process, input and output via buffer *sml*."
  (interactive)
  (switch-to-buffer-other-window
   (make-shell "sml" sml-command))
  (inferior-sml-mode))

(defun back-to-outer-indent ()
  "Unindents out to the next outer level of indentation. Bound to \e\t"
  (interactive)
  (let ((start-column (current-column))
	(indent (current-column)))
    (if (> start-column 0)
	(progn
	  (save-excursion
	    (while (>= indent start-column)
	      (if (re-search-backward "^[^\n]" nil t)
		  (setq indent (current-indentation))
		(setq indent 0))))
	  (backward-delete-char-untabify (- start-column indent))))))


(defun simulate-send-region (process point1 point2)
  "send the region (delimited by POINT1 and POINT2), a line at a time, to the 
   PROCESS"
  (interactive "sSend to process: 
r")
  (let ((newpoint point1) (incr 50))
  (save-excursion
    (while (< point1 point2)
      (setq newpoint (min (+ point1 50) point2))
      (send-region process point1 newpoint)
      (setq point1 newpoint)))))
    

(defun sml-send-defun nil
  "Send the current defun to the Sml process made by M-x run-sml."
  (interactive)
  (save-excursion
    (forward-paragraph) ;   was: (end-of-defun)
   (let ((end (point)))
     (backward-paragraph) ; was: (beginning-of-defun)
     (simulate-send-region "sml" (point) end)
     (send-string "sml" "\n"))))

(defun sml-send-defun-and-go nil
  "Send the current defun to the inferior Sml, and switch to *sml* buffer."
  (interactive)
  (sml-send-defun)
  (pop-to-buffer "*sml*")
  (end-of-buffer))

(defun sml-send-buffer nil
  "Send the whole buffer to the Sml process made by M-x run-sml."
  (interactive)
  (save-excursion
   (goto-char (point-max))		;allegedly faster than (end-of-buffer)
   (let ((end (point)))
     (goto-char (point-min))
     (simulate-send-region "sml" (point) end)
     (send-string "sml" "\n"))))

(setq auto-mode-alist (cons (cons "\\.sml" 'sml-mode) auto-mode-alist))

(setq sml-mode-is-loaded t)

