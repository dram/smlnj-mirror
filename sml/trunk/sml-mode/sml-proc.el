;;; sml-proc.el. Comint based interaction mode for Standard ML.

(defconst rcsid-sml-proc "@(#)$Name$:$Id$")

;; Copyright (C) 1989, Lars Bo Nielsen, 1994,1997 Matthew J. Morley

;; $Revision$
;; $Date$

;; ====================================================================

;; This file is not part of GNU Emacs, but it is distributed under the
;; same conditions.

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
;; Free Software Foundation, 675 Mass Ave, Cambridge, MA 0139, USA.
;; (See sml-mode.el for HISTORY.) 

;; ====================================================================

;; [MJM 10/94] Separating this from sml-mode means sml-mode will run
;; under 18.59 (or anywhere without comint, if there are such places).
;; See sml-mode.el for further information.

;;; DESCRIPTION

;; Inferior-sml-mode is for interacting with an ML process run under
;; emacs. This uses the comint package so you get history, expansion,
;; backup and all the other benefits of comint. Interaction is
;; achieved by M-x sml which starts a sub-process under emacs. You may
;; need to set this up for autoloading in your .emacs:

;; (autoload 'sml "sml-proc" "Run an inferior ML process." t)

;; Exactly what process is governed by the variable sml-program-name
;; -- just "sml" by default. If you give a prefix argument (C-u M-x
;; sml) you will be prompted for a different program to execute from
;; the default -- if you just hit RETURN you get the default anyway --
;; along with the option to specify any command line arguments. Once
;; you select the ML program name in this manner, it remains the
;; default (unless you set in a hook, or otherwise).

;; NOTE: inferior-sml-mode-hook is run AFTER the ML program has been
;; launched. inferior-sml-load-hook is run only when sml-proc.el is
;; loaded into Emacs.

;; When running an ML process some further key-bindings are effective
;; in sml-mode buffer(s). C-c C-s (switch-to-sml) will split the
;; screen into two windows if necessary and place you in the ML
;; process buffer. In the interaction buffer, C-c C-s is bound to the
;; `sml' command by default (in case you need to restart).

;; C-c C-l (sml-load-file) will load an SML source file into the
;; inferior process, C-c C-r (sml-send-region) will send the current
;; region of text to the ML process, etc. Given a prefix argument to
;; these commands will switch you from the SML buffer to the ML
;; process buffer as well as sending the text. If you get errors
;; reported by the compiler, C-c ` (sml-next-error) will step through
;; the errors with you.

;; NOTE. There is only limited support for this as it obviously
;; depends on the compiler's error messages being recognised by the
;; mode. Error reporting is currently only geared up for SML/NJ,
;; Moscow ML, and Poly/ML (see file sml-{mosml,poly-ml}.el). Look at
;; the documentation for sml-error-parser and sml-next-error -- you
;; may only need to modify the former to recover this feature for some
;; other ML systems, along with sml-error-regexp.

;; While small pieces of text can be fed quite happily into the ML
;; process directly, lager pieces should (probably) be sent via a
;; temporary file making use of the compiler's "use" command. 
;; To be safe, we always use a temp file (which also improves error
;; reporting).

;;; FOR YOUR .EMACS

;; Here  are some ideas for inferior-sml-*-hooks:

;; (setq inferior-sml-load-hook
;;       '(lambda() "Set global defaults for inferior-sml-mode"
;;          (define-key inferior-sml-mode-map "\C-cd"    'sml-cd)
;;          (define-key          sml-mode-map "\C-cd"    'sml-cd)
;;          (define-key          sml-mode-map "\C-c\C-f" 'sml-send-function)

;; (setq inferior-sml-mode-hook
;;       '(lambda() "Inferior SML mode defaults"
;;          (setq comint-scroll-show-maximum-output t
;;                comint-scroll-to-bottom-on-output t
;;                comint-input-autoexpand nil)))

;; ===================================================================

;;; INFERIOR ML MODE VARIABLES

(require 'sml-mode)
(require 'sml-util)
(require 'comint)
(require 'compile)

(defvar sml-program-name "sml"
  "*Program to run as ML.")

(defvar sml-default-arg ""
  "*Default command line option to pass, if any.")

(defvar sml-compile-command "CM.make()"
  "The command used by default by `sml-make'.")

(defvar sml-make-file-name "sources.cm"
  "The name of the makefile that `sml-make' will look for (if non-nil).")

;;(defvar sml-raise-on-error nil
;;  "*When non-nil, `sml-next-error' will raise the ML process's frame.")

(defvar inferior-sml-mode-hook nil
  "*This hook is run when the inferior ML process is started.
All buffer local customisations for the interaction buffers go here.")

(defvar inferior-sml-load-hook nil
  "*Hook run when inferior-sml-mode (sml-proc.el) is loaded into Emacs.
This is a good place to put your preferred key bindings.")

(defvar sml-error-overlay nil
  "*Non-nil means use an overlay to highlight errorful code in the buffer.
The actual value is the name of a face to use for the overlay.
Instead of setting this variable to 'region, you can also simply keep
it NIL and use (transient-mark-mode) which will provide similar
benefits (but with several side effects).")

(defvar sml-buffer nil
  "*The current ML process buffer.

MULTIPLE PROCESS SUPPORT (Whoever wants multi-process support anyway?)
=====================================================================
sml-mode supports, in a fairly simple fashion, running multiple ML
processes. To run multiple ML processes, you start the first up with
\\[sml]. It will be in a buffer named *sml*. Rename this buffer with
\\[rename-buffer]. You may now start up a new process with another
\\[sml]. It will be in a new buffer, named *sml*. You can switch
between the different process buffers with \\[switch-to-buffer].

NB *sml* is just the default name for the buffer. It actually gets
it's name from the value of `sml-program-name' -- *poly*, *smld*,...

If you have more than one ML process around, commands that send text
from source buffers to ML processes -- like `sml-send-function' or
`sml-send-region' -- have to choose a process to send it to. This is
determined by the global variable `sml-buffer'. Suppose you have three
inferior ML's running:
    Buffer      Process
    sml         #<process sml>
    mosml       #<process mosml>
    *sml*       #<process sml<2>>
If you do a \\[sml-send-function] command on some ML source code, 
what process do you send it to?

- If you're in a process buffer (sml, mosml, or *sml*), you send it to
  that process (usually makes sense only to `sml-load-file').
- If you're in some other buffer (e.g., a source file), you send it to
  the process attached to buffer `sml-buffer'.

This process selection is performed by function `sml-proc' which looks
at the value of `sml-buffer' -- which must be a lisp buffer object, or
a string \(or nil\).

Whenever \\[sml] fires up a new process, it resets `sml-buffer' to be
the new process's buffer. If you only run one process, this will do
the right thing. If you run multiple processes, you can change
`sml-buffer' to another process buffer with \\[set-variable], or
use the command \\[sml-buffer] in the interaction buffer of choice.")


;;; ALL STUFF THAT DEFAULTS TO THE SML/NJ COMPILER (0.93)

(defvar sml-use-command "use \"%s\""
  "*Template for loading a file into the inferior ML process.
Set to \"use \\\"%s\\\"\" for SML/NJ or Edinburgh ML; 
set to \"PolyML.use \\\"%s\\\"\" for Poly/ML, etc.")

(defvar sml-cd-command "OS.FileSys.chDir \"%s\""
  "*Command template for changing working directories under ML.
Set this to nil if your compiler can't change directories.

The format specifier \"%s\" will be converted into the directory name
specified when running the command \\[sml-cd].")

(defvar sml-prompt-regexp "^[-=>#] *"
  "*Regexp used to recognise prompts in the inferior ML process.")

(defvar sml-error-parser 'sml-smlnj-error-parser
  "*This function parses an error message into a 3-5 element list:

    \(file start-line start-col end-line-col err-msg\).

The first three components are required by `sml-next-error', but the other
two are optional. If the file associated with the input is the standard
input stream, this function should probably return

    \(\"std_in\" start-line start-col\).

This function will be called in a context in which the match data \(see
`match-data'\) are current for `sml-error-regexp'. The mode sets the
default value to the function `sml-smlnj-error-parser'.

In a step towards greater sml-mode modularity END-LINE-COL can be either

  - the symbol nil \(in which case it is ignored\)

or

  - an Emacs Lisp expression that when `eval'd at \(start-line,start-col\)
    will move point to the end of the errorful text in the file.

Note that the compiler should return the full path name of the errorful
file, and that this might require you to fiddle with the compiler's
prettyprinting switches.")

;; std_in:2.1-4.3 Error: operator and operand don't agree (tycon mismatch)
;; std_in:2.1 Error: operator and operand don't agree (tycon mismatch)

(defconst sml-error-regexp-alist
  '(;; Poly/ML messages
    ("\\(Error\\|Warning:\\) in '\\(.+\\)', line \\([0-9]+\\)" 2 3)
    ;; Moscow ML
    ("File \"\\([^\"]+\\)\", line \\([0-9]+\\)\\(-\\([0-9]+\\)\\)?, characters \\([0-9]+\\)-\\([0-9]+\\):" 1 2 5)
    ;; SML/NJ:  the file-pattern is restricted to no-spaces to avoid
    ;; pathological behavior with very long lines.
    ("^[-= ]*\\([^ ]+\\):\\([0-9]+\\)\\.\\([0-9]+\\)\\(-\\([0-9]+\\)\\.\\([0-9]+\\)\\)? \\(Error\\|Warning\\): .*" 1 sml-make-error 2 3 5 6)
    ;; SML/NJ's exceptions:  see above.
    ("^ +\\(raised at: \\)?\\([^ ]+\\):\\([0-9]+\\)\\.\\([0-9]+\\)\\(-\\([0-9]+\\)\\.\\([0-9]+\\)\\)" 2 sml-make-error 3 4 6 7)))

(defvar sml-error-regexp nil
  "*Regexp for matching \(the start of\) an error message.")

;; font-lock support
(defconst inferior-sml-font-lock-keywords
  `(;; prompt and following interactive command
    (,(concat "\\(" sml-prompt-regexp "\\)\\(.*\\)")
     (1 font-lock-prompt-face)
     (2 font-lock-command-face keep))
    ;; CM's messages
    ("^\\[\\(.*GC #.*\n\\)*.*\\]" . font-lock-comment-face)
    ;; SML/NJ's irritating GC messages
    ("^GC #.*" . font-lock-comment-face)
    ;; error messages
    ,@(mapcar (lambda (ra) (cons (car ra) 'font-lock-warning-face))
	      sml-error-regexp-alist))
  "Font-locking specification for inferior SML mode.")

;; default faces values
(defvar font-lock-prompt-face
  (if (facep 'font-lock-prompt-face)
      'font-lock-prompt-face
    'font-lock-keyword-face))
(defvar font-lock-command-face
  (if (facep 'font-lock-command-face)
      'font-lock-command-face
    'font-lock-function-name-face))

(defvar inferior-sml-font-lock-defaults
  '(inferior-sml-font-lock-keywords nil nil nil nil))

;;; CODE

(defmap inferior-sml-mode-map
  '(("\C-c\C-s"	. run-sml)
    ("\t"	. comint-dynamic-complete))
  "Keymap for inferior-sml mode"
  :inherit (list sml-bindings comint-mode-map))


;; buffer-local

(defvar sml-temp-file nil)
(defvar sml-error-file nil)             ; file from which the last error came
(defvar sml-error-cursor nil)           ;   ditto

(defun sml-proc-buffer ()
  "Returns the current ML process buffer,
or the current buffer if it is in `inferior-sml-mode'. Raises an error
if the variable `sml-buffer' does not appear to point to an existing
buffer."
  (or (and (eq major-mode 'inferior-sml-mode) (current-buffer))
      (and sml-buffer
	   (let ((buf (get-buffer sml-buffer)))
	     ;; buffer-name returns nil if the buffer has been killed
	     (and buf (buffer-name buf) buf)))
      ;; no buffer found, make a new one
      (run-sml t)))

(defun sml-proc ()
  "Returns the current ML process. See variable `sml-buffer'."
  (assert (eq major-mode 'inferior-sml-mode))
  (or (get-buffer-process (current-buffer))
      (progn (run-sml t) (get-buffer-process (current-buffer)))))

(defun sml-buffer (echo)
  "Make the current buffer the current `sml-buffer' if that is sensible.
Lookup variable `sml-buffer' to see why this might be useful."
  (interactive "P")
  (when (and (not echo) (eq major-mode 'inferior-sml-mode))
    (setq sml-buffer (current-buffer)))
  (message "ML process buffer is %s."
	   (or (ignore-errors (buffer-name (get-buffer sml-buffer)))
	       "undefined")))

(defun inferior-sml-mode ()
  "Major mode for interacting with an inferior ML process.

The following commands are available:
\\{inferior-sml-mode-map}

An ML process can be fired up (again) with \\[sml].

Customisation: Entry to this mode runs the hooks on `comint-mode-hook'
and `inferior-sml-mode-hook' (in that order).

Variables controlling behaviour of this mode are

`sml-program-name' (default \"sml\")
    Program to run as ML.

`sml-use-command' (default \"use \\\"%s\\\"\")
    Template for loading a file into the inferior ML process.

`sml-cd-command' (default \"System.Directory.cd \\\"%s\\\"\")
    ML command for changing directories in ML process (if possible).

`sml-prompt-regexp' (default \"^[\\-=] *\")
    Regexp used to recognise prompts in the inferior ML process.

`sml-error-regexp' 
   (default -- complicated)
    Regexp for matching error messages from the compiler.

`sml-error-parser' (default 'sml-smlnj-error-parser)
    This function parses a error messages into a 3, 4 or 5 element list:
    (file start-line start-col (end-line end-col) err-msg).

You can send text to the inferior ML process from other buffers containing
ML source.  
    `switch-to-sml' switches the current buffer to the ML process buffer.
    `sml-send-function' sends the current *paragraph* to the ML process.
    `sml-send-region' sends the current region to the ML process.

    Prefixing the sml-send-<whatever> commands with \\[universal-argument]
    causes a switch to the ML process buffer after sending the text.

For information on running multiple processes in multiple buffers, see
documentation for variable `sml-buffer'.

Commands:
RET after the end of the process' output sends the text from the 
    end of process to point.
RET before the end of the process' output copies the current line
    to the end of the process' output, and sends it.
DEL converts tabs to spaces as it moves back.
TAB file name completion, as in shell-mode, etc.."
  (interactive)
  (kill-all-local-variables)
  (comint-mode)
  (setq comint-prompt-regexp sml-prompt-regexp)
  (sml-mode-variables)

  ;; For sequencing through error messages:
  (set (make-local-variable 'sml-error-cursor) (point-max-marker))
  (set-marker-insertion-type sml-error-cursor nil)
  (set (make-local-variable 'font-lock-defaults)
       inferior-sml-font-lock-defaults)

  ;; compilation support (used for next-error)
  (set (make-local-variable 'compilation-error-regexp-alist)
       sml-error-regexp-alist)
  (compilation-shell-minor-mode 1)
  ;; I'm sure people might kill me for that
  (setq compilation-error-screen-columns nil)
  (make-local-variable 'sml-endof-error-alist)
  ;;(make-local-variable 'sml-error-overlay)

  (setq major-mode 'inferior-sml-mode)
  (setq mode-name "Inferior ML")
  (setq mode-line-process '(": %s"))
  (use-local-map inferior-sml-mode-map)
  ;;(add-hook 'kill-emacs-hook 'sml-temp-tidy)

  (run-hooks 'inferior-sml-mode-hook))

;;; FOR RUNNING ML FROM EMACS

;;;###autoload
(defun run-sml (&optional pfx)
  "Run an inferior ML process, input and output via buffer *sml*. 
With a prefix argument, this command allows you to specify any command
line options to pass to the complier. The command runs hook functions
on `comint-mode-hook' and `inferior-sml-mode-hook' in that order.

If there is a process already running in *sml*, just switch to that
buffer instead. 

In fact the name of the buffer created is chosen to reflect the name
of the program name specified by `sml-program-name', or entered at the
prompt. You can have several inferior ML process running, but only one
current one -- given by `sml-buffer' (qv).

\(Type \\[describe-mode] in the process buffer for a list of commands.)"
  (interactive "P")
  (let ((cmd (if pfx
                 (read-string "ML command: " sml-program-name)
               sml-program-name))
        (args (if pfx
                  (read-string "Any args: " sml-default-arg)
                sml-default-arg)))
    (sml-run cmd args)))

(defun sml-run (cmd arg)
  "Run the ML program CMD with given arguments ARGS.
This usually updates `sml-buffer' to a buffer named *CMD*."
  (let* ((pname (file-name-nondirectory cmd))
         (args (if (equal arg "") () (sml-args-to-list arg))))
    ;; and this -- to keep these as defaults even if
    ;; they're set in the mode hooks.
    (setq sml-program-name cmd)
    (setq sml-default-arg arg)
    (setq sml-buffer (apply 'make-comint pname cmd nil args))

    (set-buffer sml-buffer)
    (message (format "Starting \"%s\" in background." pname))
    (inferior-sml-mode)
    (goto-char (point-max))
    sml-buffer))

(defun sml-args-to-list (string)
  (let ((where (string-match "[ \t]" string)))
    (cond ((null where) (list string))
          ((not (= where 0))
           (cons (substring string 0 where)
                 (sml-args-to-list (substring string (+ 1 where)
                                              (length string)))))
          (t (let ((pos (string-match "[^ \t]" string)))
               (if (null pos)
                   nil
                   (sml-args-to-list (substring string pos
                                                (length string)))))))))

;;;###autoload 
(defun switch-to-sml (eob-p)
  "Switch to the ML process buffer.
With prefix argument, positions cursor at point, otherwise at end of buffer."
  (interactive "P")
  (pop-to-buffer (sml-proc-buffer))
  (cond ((not eob-p)
         (push-mark (point) t)
         (goto-char (point-max)))))

;; Fakes it with a "use <temp-file>;" if necessary.

;;;###autoload 
(defun sml-send-region (start end &optional and-go)
  "Send current region to the inferior ML process.
Prefix argument means switch-to-sml afterwards.

The region is written out to a temporary file and a \"use <temp-file>\" command
is sent to the compiler.
See variables `sml-use-command'."
  (interactive "r\nP")
  (if (= start end)
      (message "The region is zero (ignored)")
    (let* ((buf (sml-proc-buffer))
	   (file (buffer-file-name))
	   (marker (copy-marker start))
	   (tmp (make-temp-file "sml")))
      (write-region start end tmp nil 'silently)
      (with-current-buffer buf
	(when sml-temp-file
	  (ignore-errors (delete-file (car sml-temp-file)))
	  (set-marker (cdr sml-temp-file) nil))
	(setq sml-temp-file (cons tmp marker))
	(sml-send-string (format sml-use-command tmp) nil and-go)))))

;; This is quite bogus, so it isn't bound to a key by default.
;; Anyone coming up with an algorithm to recognise fun & local
;; declarations surrounding point will do everyone a favour!

(defun sml-send-function (&optional and-go)
  "Send current paragraph to the inferior ML process. 
With a prefix argument switch to the sml buffer as well 
\(cf. `sml-send-region'\)."
  (interactive "P")
  (save-excursion
    (sml-mark-function)
    (sml-send-region (point) (mark)))
  (if and-go (switch-to-sml nil)))

(defvar sml-source-modes '(sml-mode)
  "*Used to determine if a buffer contains ML source code.
If it's loaded into a buffer that is in one of these major modes, it's
considered an ML source file by `sml-load-file'. Used by these commands
to determine defaults.")

;;;###autoload 
(defun sml-send-buffer (&optional and-go)
  "Send buffer to inferior shell running ML process. 
With a prefix argument switch to the sml buffer as well
\(cf. `sml-send-region'\)."
  (interactive "P")
  (if (memq major-mode sml-source-modes)
    (sml-send-region (point-min) (point-max) and-go)))

;; Since sml-send-function/region take an optional prefix arg, these
;; commands are redundant. But they are kept around for the user to
;; bind if she wishes, since its easier to type C-c r than C-u C-c C-r.

(defun sml-send-region-and-go (start end)
  "Send current region to the inferior ML process, and go there."
  (interactive "r")
  (sml-send-region start end t))

(defun sml-send-function-and-go ()
  "Send current paragraph to the inferior ML process, and go there."
  (interactive)
  (sml-send-function t))


;;; Mouse control and handling dedicated frames for Inferior ML

;; simplified from frame.el in Emacs: special-display-popup-frame...

;; (defun sml-proc-frame ()
;;   "Returns the current ML process buffer's frame, or creates one first."
;;   (let ((buffer (sml-proc-buffer)))
;;     (window-frame (display-buffer buffer))))

;;; H A C K   A T T A C K !   X E M A C S   V E R S U S   E M A C S

;; Only these two functions have to dance around the inane differences 
;; between Emacs and XEmacs (fortunately)

;; (defun sml-warp-mouse (frame)
;;   "Warp the pointer across the screen to upper right corner of FRAME."
;;   (raise-frame frame)
;;   (cond ((string-match "\\(Lucid\\|XEmacs\\)" emacs-version)
;;          ;; LUCID (19.10) or later... set-m-pos needs a WINDOW
;;          (set-mouse-position (frame-root-window frame) (1- (frame-width)) 0))
;;         (t
;;          ;; GNU, post circa 19.19... set-m-pos needs a FRAME
;;          (set-mouse-position frame (1- (frame-width)) 0)
;;          ;; probably not needed post 19.29
;;          (if (fboundp 'unfocus-frame) (unfocus-frame)))))

(defun sml-drag-region (event)
  "Highlight the text the mouse is dragged over, and send it to ML.
This must be bound to a button-down mouse event, currently \\[sml-drag-region].

If you drag the mouse (ie, keep the mouse button depressed) the
program text sent to the complier is delimited by where you started
dragging the mouse, and where you release the mouse button.

If you only click the mouse, the program text sent to the compiler is
delimited by the current position of point and the place where you
click the mouse.

In either event, the values of both point and mark are left
undisturbed once this operation is completed."
  (interactive "e")
  (let ((mark-ring)                     ;BAD: selection start gets cons'd
        (pmark (point)))                ;where point is now
    (if (fboundp 'mouse-track-default)
        ;; Assume this is XEmacs, otherwise assume its Emacs
        (save-excursion
          (let ((zmacs-regions))
            (set-marker (mark-marker) nil)
            (mouse-track-default event)
            (if (not (region-exists-p)) (push-mark pmark nil t))
            (call-interactively 'sml-send-region)))
      ;; Emacs: making this buffer-local ought to happen in sml-mode
      (make-local-variable 'transient-mark-mode)
      (save-excursion
        (let ((transient-mark-mode 1))
          (mouse-drag-region event)
          (if (not mark-active) (push-mark pmark nil t))
          (call-interactively 'sml-send-region))))))


;;; LOADING AND IMPORTING SOURCE FILES:

(defvar sml-prev-dir/file nil
  "Caches the (directory . file) pair used in the last `sml-load-file'
or `sml-cd' command. Used for determining the default in the next one.")

;;;###autoload 
(defun sml-load-file (&optional and-go)
  "Load an ML file into the current inferior ML process. 
With a prefix argument switch to sml buffer as well.

This command uses the ML command template `sml-use-command' to construct
the command to send to the ML process\; a trailing \"\;\\n\" will be added
automatically."
  (interactive "P")
  (let ((file (car (comint-get-source
		    "Load ML file: " sml-prev-dir/file sml-source-modes t))))
    (with-current-buffer (sml-proc-buffer)
      ;; Check if buffer needs saved. Should (save-some-buffers) instead?
      (comint-check-source file)
      (setq sml-prev-dir/file
	    (cons (file-name-directory file) (file-name-nondirectory file)))
      (sml-send-string (format sml-use-command file) nil and-go))))

(defun sml-cd (dir)
  "Change the working directory of the inferior ML process.
The default directory of the process buffer is changed to DIR. If the
variable `sml-cd-command' is non-nil it should be an ML command that will
be executed to change the compiler's working directory\; a trailing
\"\;\\n\" will be added automatically."
  (interactive "DSML Directory: ")
  (let ((dir (expand-file-name dir)))
    (with-current-buffer (sml-proc-buffer)
      (sml-send-string (format sml-cd-command dir) t)
      (setq default-directory dir))
    (setq sml-prev-dir/file (cons dir nil))))

(defun sml-send-string (str &optional print and-go)
  (let ((proc (sml-proc))
	(str (concat str ";\n"))
	(win (get-buffer-window (current-buffer) 'visible)))
    (when win (select-window win))
    (goto-char (point-max))
    (when print (insert str))
    (sml-update-cursor)
    (set-marker (process-mark proc) (point-max))
    (setq compilation-last-buffer (current-buffer))
    (comint-send-string proc str)
    (when and-go (switch-to-sml nil))))

(defun sml-compile (command)
  "re-make a system using (by default) CM.
   The exact command used can be specified by providing a prefix argument."
  (interactive
   ;; code taken straight from compile.el
   (if (or compilation-read-command current-prefix-arg)
       (list (read-from-minibuffer "Compile command: "
                                 sml-compile-command nil nil
                                 '(compile-history . 1)))
     (list sml-compile-command)))
  (setq sml-compile-command command)
  (save-some-buffers (not compilation-ask-about-save) nil)
  ;; try to find a makefile up the directory tree
  (let ((dir (when sml-make-file-name default-directory)))
    (while (and dir (not (file-exists-p (concat dir sml-make-file-name))))
      (let ((newdir (file-name-directory (directory-file-name dir))))
	(setq dir (unless (equal newdir dir) newdir))))
    (unless dir (setq dir default-directory))
    (with-current-buffer (sml-proc-buffer)
      (setq default-directory dir)
      (sml-send-string (concat (format sml-cd-command dir) "; " command) t t))))

;;; PARSING ERROR MESSAGES

;; This should need no modification to support other compilers. 

;; Update the buffer-local error-cursor in proc-buffer to be its
;; current proc mark.

(defvar sml-endof-error-alist nil)

(defun sml-update-cursor ()
  ;; update buffer local variable
  (set-marker sml-error-cursor (1- (process-mark (sml-proc))))
  (setq sml-endof-error-alist nil)
  (compilation-forget-errors)
  (setq compilation-parsing-end sml-error-cursor))

(defun sml-make-error (f c)
  (let ((err (point-marker))
	(linenum (string-to-number c))
	(filename (list (first f) (second f)))
	(column (string-to-number (compile-buffer-substring (third f)))))
    ;; record the end of error, if any
    (when (fourth f)
      (let* ((endline (string-to-number (compile-buffer-substring (fourth f))))
	     (endcol (string-to-number (compile-buffer-substring (fifth f))))
	     (linediff (- endline linenum)))
	(push (list err linediff (if (= 0 linediff) (- endcol column) endcol))
	      sml-endof-error-alist)))
    ;; build the error descriptor
    (if (string= (car sml-temp-file) (first f))
	;; special case for code sent via sml-send-region
	(let ((marker (cdr sml-temp-file)))
	  (with-current-buffer (marker-buffer marker)
	    (goto-char marker)
	    (forward-line (1- linenum))
	    (forward-char (1- column))
	    (cons err (point-marker))))
      ;; taken from compile.el
      (list err filename linenum column))))

(defadvice compilation-goto-locus (after sml-endof-error activate)
  (let* ((next-error (ad-get-arg 0))
	 (err (car next-error))
	 (pos (cdr next-error))
	 (endof (with-current-buffer (marker-buffer err)
		  (assq err sml-endof-error-alist))))
    (if (not endof) (sml-error-overlay 'undo)
      (with-current-buffer (marker-buffer pos)
	(goto-char pos)
	(let ((linediff (second endof))
	      (coldiff (third endof)))
	  (when (> 0 linediff) (forward-line linediff))
	  (forward-char coldiff))
	(sml-error-overlay nil pos (point))
	(push-mark nil t (not sml-error-overlay))
	(goto-char pos)))))

(defun sml-error-overlay (undo &optional beg end)
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
  (when sml-error-overlay
    (unless (overlayp sml-error-overlay)
      (let ((ol sml-error-overlay))
	(setq sml-error-overlay (make-overlay 0 0))
	(overlay-put sml-error-overlay 'face (if (symbolp ol) ol 'region))))
    (if undo
	(move-overlay sml-error-overlay 1 1 (current-buffer))
      ;; if active regions, signals mark not active if no region set
      (let ((beg (or beg (region-beginning)))
	    (end (or end (region-end))))
	(move-overlay sml-error-overlay beg end (current-buffer))))))

;; ;;;###autoload 
;; (defun sml-next-error (skip)
;;   "Find the next error by parsing the inferior ML buffer. 
;; A prefix argument means `sml-skip-errors' (qv) instead.

;; Move the error message on the top line of the window\; put the cursor
;; \(point\) at the beginning of the error source.

;; If the error message specifies a range, and `sml-error-parser' returns
;; the range, the mark is placed at the end of the range. If the variable
;; `sml-error-overlay' is non-nil, the region will also be highlighted.

;; If `sml-error-parser' returns a fifth component this is assumed to be
;; a string to indicate the nature of the error: this will be echoed in
;; the minibuffer.

;; Error interaction only works if there is a real file associated with
;; the input -- though of course it also depends on the compiler's error
;; messages \(also see documantation for `sml-error-parser'\).

;; However: if the last text sent went via `sml-load-file' (or the temp
;; file mechanism), the next error reported will be relative to the start
;; of the region sent, any error reports in the previous output being
;; forgotten. If the text went directly to the compiler the succeeding
;; error reported will be the next error relative to the location \(in
;; the output\) of the last error. This odd behaviour may have a use...?"
;;   (interactive "P")
;;   (if skip (sml-skip-errors) (sml-do-next-error)))

;; (defun sml-do-next-error ()
;;   "The business end of `sml-next-error' (qv)"
;;   (let ((case-fold-search nil)
;;         ;; set this variable iff we called sml-next-error in a SML buffer
;;         (sml-window (if (memq major-mode sml-source-modes) (selected-window)))
;;         (proc-buffer (sml-proc-buffer)))
;;     ;; undo (don't destroy) the previous overlay to be tidy
;;     (sml-error-overlay 'undo 1 1
;;                        (and sml-error-file (get-file-buffer sml-error-file)))
;;     ;; go to interaction buffer but don't raise it's frame 
;;     (pop-to-buffer (sml-proc-buffer))
;;     ;; go to the last remembered error, and search for the next one.
;;     (goto-char sml-error-cursor)
;;     (if (not (re-search-forward sml-error-regexp (point-max) t))
;;         ;; no more errors -- move point to the sml prompt at the end
;;         (progn
;;           (goto-char (point-max))
;;           (if sml-window (select-window sml-window)) ;return there, perhaps
;;           (message "No error message(s) found."))
;;       ;; error found: point is at end of last match; set the cursor posn.
;;       (set-marker sml-error-cursor (point))
;;       ;; move the SML window's text up to this line
;;       (set-window-start (get-buffer-window proc-buffer) (match-beginning 0))
;;       (let* ((pos)
;;              (parse (funcall sml-error-parser (match-beginning 0)))
;;              (file (nth 0 parse))
;;              (line0 (nth 1 parse))
;;              (col0 (nth 2 parse))
;;              (line/col1 (nth 3 parse))
;;              (msg (nth 4 parse)))
;;         ;; Give up immediately if the error report is scribble
;;         (if (or (null file) (null line0))
;;             (error "Failed to parse/locate this error properly!"))
;;         ;; decide what to do depending on the file returned
;;         (when (string= file "std_in")
;; 	  ;; presently a fundamental limitation i'm afraid.
;; 	  (error "Sorry, can't locate errors on std_in."))
;; 	;; jump to the beginning
;; 	(if (string= file (car sml-temp-file))
;; 	    (let* ((maker (cdr sml-temp-file))
;; 		   (buf (marker-buffer marker)))
;; 	      (display-buffer buf)
;; 	      (set-buffer buf)
;; 	      (goto-char marker))
;; 	  (unless (file-readable-p file) (error "Can't read %s" file))
;;           ;; instead of (find-file-other-window file) to lookup the file
;;           (find-file-other-window file)
;;           ;; no good if the buffer's narrowed, still...
;;           (goto-char (point-min)))
;; 	;; jump to the error
;; 	(forward-line (1- line0))
;; 	(forward-char (1- col0))
;; 	;; point is at start of error text; seek the end.
;; 	(let ((start (point))
;; 	      (end (and line/col1
;; 			(condition-case nil
;; 			    (progn (eval line/col1) (point))
;; 			  (error nil)))))
;; 	  ;; return to start anyway
;; 	  (goto-char start)
;; 	  ;; if point went to end, put mark there, and maybe highlight
;; 	  (if end (progn (push-mark end t)
;; 			 (sml-error-overlay nil start end)))
;; 	  (setq sml-error-file file)   ; remember this for next time
;; 	  (if msg (message msg))))))) ; echo the error/warning message

;; (defun sml-skip-errors ()
;;   "Skip past the rest of the errors."
;;   (interactive)
;;   (if (memq major-mode sml-source-modes) (sml-error-overlay 'undo))
;;   (with-current-buffer (sml-proc-buffer) (sml-update-cursor))
;;   (if (eq major-mode 'sml-inferior-mode) (goto-char (point-max))))

;;; H A C K   A T T A C K !   X E M A C S   /   E M A C S   K E Y S

(if window-system
    (cond ((string-match "\\(Lucid\\|XEmacs\\)" emacs-version)
	   ;; LUCID (19.10) or later...
	   (define-key sml-mode-map '(meta shift button1) 'sml-drag-region))
	  (t
	   ;; GNU, post circa 19.19
	   (define-key sml-mode-map [M-S-down-mouse-1] 'sml-drag-region))))

;;; ...and do the user's customisations.

(run-hooks 'inferior-sml-load-hook)

;;; Here is where sml-proc.el ends
(provide 'sml-proc)
