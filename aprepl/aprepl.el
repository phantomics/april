;;; aprepl.el --- interaction mode for April APL  -*- lexical-binding: t -*-

;; Copyright (C) 2022 Andrew Sengul

;; Author: Andrew Sengul
;; Created: 22 Nov 2022
;; Keywords: lisp, apl, april

;; This file is licensed under the GNU General Public License v3.0.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Provides a REPL interface to the April APL compiler.

;; To start: M-x aprepl.

;;; Code:

(require 'comint)
(require 'pp)
(require 'subr-x)

;;; User variables

;; (defgroup aprepl nil
;;   "Interaction mode for Emacs Lisp."
;;   :group 'lisp)

(defgroup aprepl nil
  "Interaction mode for Emacs Lisp."
  :group 'apl)

(defcustom aprepl-noisy t
  "If non-nil, ApREPL will beep on error."
  :type 'boolean
  :group 'aprepl)

(defcustom aprepl-prompt-read-only t
  "If non-nil, the ApREPL prompt is read only.
The read only region includes the newline before the prompt.
Setting this variable does not affect existing ApREPL runs.
This works by setting the buffer-local value of `comint-prompt-read-only'.
Setting that value directly affects new prompts in the current buffer.

If this option is enabled, then the safe way to temporarily
override the read-only-ness of APRIL prompts is to call
`comint-kill-whole-line' or `comint-kill-region' with no
narrowing in effect.  This way you will be certain that none of
the remaining prompts will be accidentally messed up.  You may
wish to put something like the following in your init file:

\(add-hook \\='aprepl-mode-hook
          (lambda ()
             (define-key aprepl-map \"\\C-w\" \\='comint-kill-region)
             (define-key aprepl-map [C-S-backspace]
               \\='comint-kill-whole-line)))

If you set `comint-prompt-read-only' to t, you might wish to use
`comint-mode-hook' and `comint-mode-map' instead of
`aprepl-mode-hook' and `aprepl-map'.  That will affect all comint
buffers, including ApREPL buffers.  If you sometimes use ApREPL on
text-only terminals or with `emacs -nw', you might wish to use
another binding for `comint-kill-whole-line'."
  :type 'boolean
  :group 'aprepl
  :version "22.1")

(defcustom aprepl-prompt "      "
  "Prompt used in ApREPL.  Six spaces as per APL convention. 
Setting this variable does not affect existing ApREPL runs."
  :type 'string
  :group 'aprepl)

(defvar aprepl-prompt-internal "      "
  "Stored value of `aprepl-prompt' in the current ApREPL buffer.
This is an internal variable used by ApREPL.  Its purpose is to
prevent a running ApREPL process from being messed up when the user
customizes `aprepl-prompt'.")

(defcustom aprepl-indent "  "
  "Indentation used in ApREPL. Two spaces, intended for indenting within dfns."
  :type 'string
  :group 'aprepl)

(defvar aprepl-indent-internal "  "
  "Stored value of `aprepl-indent'.")

(defcustom aprepl-dynamic-return t
  "Controls whether \\<aprepl-map>\\[aprepl-return] has intelligent behavior in ApREPL.
If non-nil, \\[aprepl-return] evaluates input for complete sexps, or inserts a newline
and indents for incomplete sexps.  If nil, always inserts newlines."
  :type 'boolean
  :group 'aprepl)

(defcustom aprepl-dynamic-multiline-inputs t
  "Force multiline inputs to start from column zero?
If non-nil, after entering the first line of an incomplete sexp, a newline
will be inserted after the prompt, moving the input to the next line.
This gives more frame width for large indented sexps, and allows functions
such as `edebug-defun' to work with such inputs."
  :type 'boolean
  :group 'aprepl)

(defvar aprepl-closure-balance-braces 0
  "A balance count of {} braces. Used to track whether a defn is in the process of being written.")

(defvar aprepl-closure-balance-brackets 0
  "A balance count of [] brackets. Used to track whether a multiline axis is in the process of being written, as for the $[] k-style if statements.")

(defvar aprepl-stored-multiline-input ""
  "A string of multiline input, constructed as a user enters a dfn or multiline statement.")

;; (defvaralias 'april-apl-repl-mode-hook 'aprepl-mode-hook)

;; (defcustom aprepl-mode-hook nil
;;   "Hooks to be run when ApREPL (`april-apl-repl-mode') is started."
;;   :options '() ; '(eldoc-mode)
;;   :type 'hook
;;   :group 'aprepl)

(defvar aprepl-match-data nil
  "Match data saved at the end of last command.")

;;; System variables

(defvar aprepl-working-buffer nil
  "Buffer in which ApREPL expressions will be evaluated.")

(defvar aprepl-header
  "*** Welcome to the April REPL *** \n"
  "Message to display when April REPL is started.")

(defvar aprepl-default-workspace "common"
  "Default workspace for APL evaluation.")
  
(defvar aprepl-workspace "common"
  "Workspace currently in use for APL evaluation.")

(defvaralias 'april-apl-repl-mode-map 'aprepl-map)
(defvar aprepl-map
  (let ((map (make-sparse-keymap)))
    ;; (define-key map "\t" 'aprepl-tab)
    (define-key map "\C-m" 'aprepl-return)
    (define-key map "\e\C-m" 'aprepl-return-for-effect)
    (define-key map "\C-j" 'aprepl-send-input)
    (define-key map "\e\C-x" 'eval-defun)         ; for consistency with
    ;; These bindings are from `lisp-mode-shared-map' -- can you inherit
    ;; from more than one keymap??
    (define-key map "\e\C-q" 'indent-sexp)
    (define-key map "\177" 'backward-delete-char-untabify)
    ;; Some convenience bindings for setting the working buffer
    (define-key map "\C-c\C-b" 'aprepl-change-working-buffer)
    (define-key map "\C-c\C-f" 'aprepl-display-working-buffer)
    (define-key map "\C-c\C-v" 'aprepl-print-working-buffer)
    map)
  "Keymap for ApREPL mode.")

(easy-menu-define aprepl-menu aprepl-map
  "ApREPL mode menu."
  '("ApREPL"
    ["Change Working Buffer" aprepl-change-working-buffer t]
    ["Display Working Buffer" aprepl-display-working-buffer t]
    ["Print Working Buffer" aprepl-print-working-buffer t]))

(defvar aprepl-font-lock-keywords
  '(("\\(^\\*\\*\\*[^*]+\\*\\*\\*\\)\\(.*$\\)"
     (1 font-lock-comment-face)
     (2 font-lock-constant-face)))
  "Additional expressions to highlight in ApREPL buffers.")

;;; Completion stuff

;; (defun aprepl-tab ()
;;   "Indent or complete."
;;   (interactive)
;;   (if (or (eq (preceding-char) ?\n)
;;           (eq (char-syntax (preceding-char)) ?\s))
;;       (aprepl-indent-line)))

;; (defun aprepl-indent-line nil
;;   "Indent the current line as Lisp code if it is not a prompt line."
;;   (when (save-excursion (comint-bol t) (bolp))
;;     (lisp-indent-line)))

;;; Working buffer manipulation

(defun aprepl-print-working-buffer nil
  "Print the current ApREPL working buffer's name in the echo area."
  (interactive)
  (message "The current working buffer is: %s" (buffer-name aprepl-working-buffer)))

(defun aprepl-display-working-buffer nil
  "Display the current ApREPL working buffer.
Don't forget that selecting that buffer will change its value of `point'
to its value of `window-point'!"
  (interactive)
  (display-buffer aprepl-working-buffer)
  (aprepl-print-working-buffer))

(defun aprepl-change-working-buffer (buf)
  "Change the current ApREPL working buffer to BUF.
This is the buffer in which all sexps entered at the ApREPL prompt are
evaluated.  You can achieve the same effect with a call to
`set-buffer' at the ApREPL prompt."
  (interactive "bSet working buffer to: ")
  (let ((buffer (get-buffer buf)))
    (if (and buffer (buffer-live-p buffer))
        (setq aprepl-working-buffer buffer)
      (error "No such buffer: %S" buf)))
  (aprepl-print-working-buffer))

;;; Other bindings

(defun aprepl-return (&optional for-effect)
  "Newline and indent, or evaluate the sexp before the prompt.
Complete sexps are evaluated; for incomplete sexps inserts a newline
and indents.  If however `aprepl-dynamic-return' is nil, this always
simply inserts a newline."
  (interactive)
  (if aprepl-dynamic-return
      (let ((state
             (save-excursion
               (end-of-line)
               (parse-partial-sexp (aprepl-pm)
                                   (point)))))
        ;; (print (list :st state))
        (if (and (< (car state) 1) (not (nth 3 state)))
            (aprepl-send-input for-effect)
          (when (and aprepl-dynamic-multiline-inputs
                     (save-excursion
                       (beginning-of-line)
                       (looking-at-p comint-prompt-regexp)))
            (save-excursion
              (goto-char (aprepl-pm))
              (newline 1)))
          (newline-and-indent)))
    (newline)))

(defun aprepl-return-for-effect ()
  "Like `aprepl-return', but do not print the result."
  (interactive)
  (aprepl-return t))

(defvar aprepl-input)

(defun aprepl-input-sender (_proc input)
  ;; Just sets the variable aprepl-input, which is in the scope of
  ;; `aprepl-send-input's call.
  (setq aprepl-input input))

(defun aprepl-send-input (&optional for-effect)
  "Evaluate the Emacs Lisp expression after the prompt."
  (interactive)
  (let (aprepl-input)                   ; set by aprepl-input-sender
    (comint-send-input)                 ; update history, markers etc.
    (aprepl-eval-input aprepl-input for-effect)))

;;; Utility functions

;; (defun aprepl-is-whitespace-or-comment (string)
;;   "Return non-nil if STRING is all whitespace or a comment."
;;   (or (string= string "")
;;       (string-match-p "\\`[ \t\n]*\\(?:;.*\\)*\\'" string)))

;;; Evaluation

(defun aprepl-standard-output-impl (process)
  "Return a function to use for `standard-output' while in aprepl eval.
The returned function takes one character as input.  Passing nil
to this function instead of a character flushes the output
buffer.  Passing t appends a terminating newline if the buffer is
nonempty, then flushes the buffer."
  ;; Use an intermediate output buffer because doing redisplay for
  ;; each character we output is too expensive.  Set up a flush timer
  ;; so that users don't have to wait for whole lines to appear before
  ;; seeing output.
  (let* ((output-buffer nil)
         (flush-timer nil)
         (flush-buffer
          (lambda ()
            (comint-output-filter
             process
             (apply #'string (nreverse output-buffer)))
            (redisplay)
            (setf output-buffer nil)
            (when flush-timer
              (cancel-timer flush-timer)
              (setf flush-timer nil)))))
    (lambda (char)
      (let (flush-now)
        (cond ((and (eq char t) output-buffer)
               (push ?\n output-buffer)
               (setf flush-now t))
              ((characterp char)
               (push char output-buffer)))
        (if flush-now
            (funcall flush-buffer)
          (unless flush-timer
            (setf flush-timer (run-with-timer 0.1 nil flush-buffer))))))))

(defun aprepl-complete-output (output)
  (insert output ?\n)
  (insert output aprepl-prompt-internal)
  (aprepl-set-pm (point-max)))

(defun aprepl-eval-input (input-string &optional for-effect)
  "Evaluate the Lisp expression INPUT-STRING, and pretty-print the result."
  ;; This is the function that actually `sends' the input to the
  ;; `April APL compiler'. All comint-send-input does is works out
  ;; what that input is.  What this function does is evaluates that
  ;; input and produces `output' which gets inserted into the buffer,
  ;; along with a new prompt.
  ;; TODO: the string-trim-left is inefficient, can the ) position be found?
  (let ((string (string-trim-left
                 input-string))        ; input expression, as a string
        form                           ; form to evaluate
        pos                            ; End posn of parse in string
        result                         ; Result, or error message
        error-type                     ; string, nil if no error
        (output "")                    ; result to display
        (wbuf aprepl-working-buffer)   ; current buffer after evaluation
        (command-start-char ?\))
        (pmark (aprepl-pm))
        terminated)
    (if (char-equal command-start-char (aref string 0))
        (let* ((elements (delete "" (split-string (downcase (substring string 1)) "\s")))
               (command (car elements))
               (args (cdr elements)))
          (cond ((string-equal command "setws")
                 (let ((wsname (car args)))
                   (setq aprepl-workspace wsname)
                   (insert (format "Now using workspace ｢%s｣." (upcase wsname)))
                   (aprepl-complete-output output)))
                ((string-equal command "makews")
                 (let ((wsname (car args)))
                   (slime-eval-async `(swank:eval-and-grab-output
                                       ,(substring-no-properties
                                         (format "(april:april-create-workspace %s)" wsname)))
                     (lambda (result)
                       (cl-destructuring-bind (out value) result
                         (insert output (substring value 1 -1))
                         (aprepl-complete-output output))))))
                ((string-equal command "off")
                 (setq terminated t)
                 (princ "April REPL buffer closed.")
                 (kill-buffer aprepl-working-buffer))
                (t (insert output "Unknown command.")
                   (aprepl-complete-output output))))
      (let* ((has-backslashes (string-match "[\\]" string))
             (string (if (not has-backslashes) string
                       (replace-regexp-in-string "[\\]" "\\\\\\\\" string)))
             (str-index 0))
        
        (while (< str-index (length string))
          (cond ((char-equal ?\{ (aref string str-index))
                 (setq aprepl-closure-balance-braces
                       (1+ aprepl-closure-balance-braces)))
                ((char-equal ?\} (aref string str-index))
                 (setq aprepl-closure-balance-braces
                       (max 0 (1- aprepl-closure-balance-braces))))
                ((char-equal ?\[ (aref string str-index))
                 (setq aprepl-closure-balance-braces
                       (1+ aprepl-closure-balance-brackets)))
                ((char-equal ?\] (aref string str-index))
                 (setq aprepl-closure-balance-braces
                       (max 0 (1- aprepl-closure-balance-brackets)))))
          (setq str-index (1+ str-index)))

        (if (and (= 0 aprepl-closure-balance-braces)
                 (= 0 aprepl-closure-balance-brackets))
            (slime-eval-async
                `(swank:eval-and-grab-output
                  ,(substring-no-properties
                    (format "(april:april (with (:space %s) (:state :output-printed :only)) \"%s\")"
                            aprepl-workspace (concat aprepl-stored-multiline-input string))))
              (lambda (result)
                (cl-destructuring-bind (out value) result
                  (push-mark)
                  (setq aprepl-stored-multiline-input "")
                  (insert output out)
                  ;; don't insert the extra newline when there's no output, i.e. for assignments
                  (when (< 2 (length value))
                    (insert output (substring
                                    value 1 (if (char-equal ?\n (aref value (+ -2 (length value))))
                                                -2 -1)))
                    (insert output ?\n))
                  (insert output aprepl-prompt-internal)
                  (aprepl-set-pm (point-max)))))
          (progn (setf aprepl-stored-multiline-input
                       (concat aprepl-stored-multiline-input string "\n"))
                 (insert output aprepl-prompt-internal)
                 (dotimes (x aprepl-closure-balance-braces)
                   (insert output aprepl-indent))
                 (dotimes (x aprepl-closure-balance-brackets)
                   (insert output aprepl-indent))
                 (aprepl-set-pm (point-max))))))
    (when (not terminated)
      (comint-output-filter (aprepl-process) output))))

;; (while (and (= 0 input-index)
;;             (< str-index (length input-string)))
;;   (when (not (char-equal ?\  (aref input-string str-index)))
;;     (setq input-index str-index))
;;   (setq str-index (1+ str-index)))

;;; Process and marker utilities

(defun aprepl-process nil
  ;; Return the current buffer's process.
  (get-buffer-process (current-buffer)))

(defun aprepl-pm nil
  ;; Return the process mark of the current buffer.
  (process-mark (get-buffer-process (current-buffer))))

(defun aprepl-set-pm (pos)
  ;; Set the process mark in the current buffer to POS.
  (set-marker (process-mark (get-buffer-process (current-buffer))) pos))

(defcustom aprepl-load-april-on-slime-start nil
  "Should April be automatically loaded when Slime starts?
This is meant to be checked when the Slime hook for loading April is run."
  :type 'boolean
  :group 'aprepl)

;;; Major mode

(define-derived-mode april-apl-repl-mode comint-mode "APREPL"
  "Major mode for interactively evaluating April APL expressions.
Uses the interface provided by `comint-mode' (which see).

* \\[aprepl-send-input] evaluates the expression following the prompt.

* \\[aprepl-return] inserts a newline and indents, or evaluates a
  complete expression (but see variable `aprepl-dynamic-return').
  Inputs longer than one line are moved to the line following the
  prompt (but see variable `aprepl-dynamic-multiline-inputs').

* \\[aprepl-return-for-effect] works like `aprepl-return', except
  that it doesn't print the result of evaluating the input.  This
  functionality is useful when forms would generate voluminous
  output.

The current working buffer may be changed (with a call to `set-buffer',
or with \\[aprepl-change-working-buffer]), and its value is preserved between successive
evaluations.  In this way, expressions may be evaluated in a different
buffer than the *aprepl* buffer.  By default, its name is shown on the
mode line; you can always display it with \\[aprepl-print-working-buffer], or the buffer itself
with \\[aprepl-display-working-buffer].


If, at the start of evaluation, `standard-output' is t (the
default), `standard-output' is set to a special function that
causes output to be directed to the ApREPL buffer.
`standard-output' is restored after evaluation unless explicitly
set to a different value during evaluation.  You can use (princ
VALUE) or (pp VALUE) to write to the ApREPL buffer.

The behavior of ApREPL may be customized with the following variables:
* To stop beeping on error, set `aprepl-noisy' to nil.
* If you don't like the prompt, you can change it by setting `aprepl-prompt'.
* If you do not like that the prompt is (by default) read-only, set
  `aprepl-prompt-read-only' to nil.
* Set `aprepl-dynamic-return' to nil for bindings like `lisp-interaction-mode'.
* Entry to this mode runs `comint-mode-hook' and `aprepl-mode-hook'
 (in that order).

Customized bindings may be defined in `aprepl-map', which currently contains:
\\{aprepl-map}"
  :syntax-table emacs-lisp-mode-syntax-table

  (setq comint-prompt-regexp (concat "^" (regexp-quote aprepl-prompt)))
  (set (make-local-variable 'paragraph-separate) "\\'")
  (set (make-local-variable 'paragraph-start) comint-prompt-regexp)
  (setq comint-input-sender 'aprepl-input-sender)
  (setq comint-process-echoes nil)
  ;; (add-function :before-until (local 'eldoc-documentation-function)
  ;;               #'elisp-eldoc-documentation-function)
  (set (make-local-variable 'aprepl-prompt-internal) aprepl-prompt)
  (set (make-local-variable 'comint-prompt-read-only) aprepl-prompt-read-only)
  (setq comint-get-old-input 'aprepl-get-old-input)
  (set (make-local-variable 'comint-completion-addsuffix) '("/" . ""))
  (setq mode-line-process '(":%s on " (:eval (buffer-name aprepl-working-buffer))))
  ;; Useful for `hs-minor-mode'.
  (setq-local comment-start "⍝")
  (setq-local comment-use-syntax t)

  ;; (set (make-local-variable 'indent-line-function) 'aprepl-indent-line)
  (set (make-local-variable 'aprepl-working-buffer) (current-buffer))
  (set (make-local-variable 'fill-paragraph-function) 'lisp-fill-paragraph)

  (set (make-local-variable 'aprepl-match-data) nil)

  ;; font-lock support
  (set (make-local-variable 'font-lock-defaults)
       '(aprepl-font-lock-keywords nil nil ((?: . "w") (?- . "w") (?* . "w"))))

  ;; A dummy process to keep comint happy. It will never get any input
  (unless (comint-check-proc (current-buffer))
    ;; Was cat, but on non-Unix platforms that might not exist, so
    ;; use hexl instead, which is part of the Emacs distribution.
    (condition-case nil
        (start-process "aprepl" (current-buffer) "hexl")
      (file-error (start-process "aprepl" (current-buffer) "cat")))
    (set-process-query-on-exit-flag (aprepl-process) nil)
    (goto-char (point-max))

    ;; initial workspace is the default workspace
    (setq aprepl-workspace aprepl-default-workspace)
    
    ;; Lisp output can include raw characters that confuse comint's
    ;; carriage control code.
    (set (make-local-variable 'comint-inhibit-carriage-motion) t)

    (insert "\n\n")
    (slime-eval-async ;; print header selected and formatted with April banner function
        `(swank:eval-and-grab-output
          ,(substring-no-properties
            (format "(april::display-banner :width %s)" (floor (* 0.85 (window-total-width))))))
      (lambda (result)
        (cl-destructuring-bind (out value) result
          (insert (substring value 1 -1))
          (insert "\n\n")
          (aprepl-set-pm (point-max))
          (unless comint-use-prompt-regexp
            (let ((inhibit-read-only t))
              (add-text-properties
               (point-min) (point-max)
               '(rear-nonsticky t field output inhibit-line-move-field-capture t))))
          (comint-output-filter (aprepl-process) aprepl-prompt-internal)
          (set-marker comint-last-input-start (aprepl-pm))
          (set-process-filter (get-buffer-process (current-buffer)) 'comint-output-filter))))))

(defun aprepl-get-old-input nil
  ;; Return the previous input surrounding point
  (save-excursion
    (beginning-of-line)
    (unless (looking-at-p comint-prompt-regexp)
      (re-search-backward comint-prompt-regexp))
    (comint-skip-prompt)
    (buffer-substring (point) (progn (forward-sexp 1) (point)))))

;; (slime-eval-async
;;     `(swank:eval-and-grab-output ,(substring-no-properties
;;                                    "(asdf:find-system 'april nil)"))
;;   (lambda (result)
;;     (cl-destructuring-bind (out value) result
;;       (print value)
;;       (print 36))))

;; (defun aprepl-prep-system-load-april ()
;;   (print "eee")
;;   (message "Trying to load")
;;   (slime-eval-async
;;       `(swank:eval-and-grab-output ,(substring-no-properties
;;                                      "(asdf:find-system 'april nil)"))
;;     (lambda (result)
;;       (cl-destructuring-bind (out value) result
;;         (print (list :res result value (upcase value)))
;;         (if (not (string= "NIL" (upcase value)))
;;             t (let ((april-load-selection (read-string "April compiler not loaded. Load it? [y/n] : ")))
;;                 (print (list :bla))
;;                 (when (char-equal ?y (aref april-load-selection 0))
;;                   (slime-eval-async `(swank:eval-and-grab-output ,(substring-no-properties
;;                                                                    "(asdf:load-system 'april)"))
;;                     (lambda (result)
;;                       (cl-destructuring-bind (out value) result
;;                         (print (list :val value))))))))))))

;; (defun aprepl-prep-system-load-april ()
;;   (slime-eval-async `(swank:eval-and-grab-output ,(substring-no-properties
;;                                                    "(asdf:load-system 'april)"))
;;     (lambda (result)
;;       (cl-destructuring-bind (out value) result
;;         (print (list :val2 value))
;;         t))))

;; (aprepl-prep-system-load-april)

;; (add-hook 'slime-connected-hook 'aprepl-prep-system-load-april)

;; (defun aprepl-prep-system (next-do)
;;   (if (slime-connected-p)
;;       (slime-eval-async
;;           `(swank:eval-and-grab-output ,(substring-no-properties
;;                                          "(asdf:find-system 'april nil)"))
;;         (lambda (result)
;;           (cl-destructuring-bind (out value) result
;;             (print (list :res result value (upcase value)))
;;             (if (not (string= "NIL" (upcase value)))
;;                 t (let ((april-load-selection (read-string "April compiler not loaded. Load it? [y/n] : ")))
;;                     (when (char-equal ?y (aref april-load-selection 0))
;;                       (aprepl-prep-system-load-april)))))))
;;     (let ((start-slime-selection (read-string "Slime is not running. Start it? [y/n] : ")))
;;       (when (char-equal ?y (aref start-slime-selection 0))
;;         (slime)))))

;; Check whether REPL is able to run

(defun aprepl-check-system-ready-then (do-next)
  "Check whether it's possible to run ApREPL, i.e. make sure that Slime is running and April's system is loaded."
  (if (slime-connected-p)
      (slime-eval-async
          `(swank:eval-and-grab-output "(asdf:find-system 'april nil)")
        (lambda (result)
          (cl-destructuring-bind (out value) result
            (if (string= "NIL" (upcase value))
                (princ "Slime is running but April is not loaded. Please evaluate (asdf:load-system 'april) in the Slime REPL.")
              (funcall do-next)))))
    (princ "Slime is not running. Please start it, i.e. with M-x slime.")))

;;; User command

;;;###autoload
(defun aprepl (&optional buf-name)
  "Interactively evaluate April APL expressions.
Switches to the buffer named BUF-NAME if provided (`*aprepl*' by default),
or creates it if it does not exist."
  (interactive)
  (aprepl-check-system-ready-then
   (lambda ()
     (let (old-point (buf-name (or buf-name "*aprepl*")))
       (unless (comint-check-proc buf-name)
         (with-current-buffer (get-buffer-create buf-name)
           (unless (zerop (buffer-size)) (setq old-point (point)))
           (april-apl-repl-mode)))
       (pop-to-buffer-same-window buf-name)
       (when old-point (push-mark old-point))))))

(provide 'aprepl)

;;; aprepl.el ends here
