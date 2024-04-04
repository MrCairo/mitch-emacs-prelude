;;; init-helpful.el -- Configure the package
;;;
;;; Commentary:
;;; [[https://github.com/Wilfred/helpful][Helpful]] adds a lot of very helpful
;;; (get it?) information to Emacs' =describe-= command buffers.  For example,
;;; if you use =describe-function=, you will not only get the documentation
;;; about the function, you will also see the source code of the function and
;;; where it gets used in other places in the Emacs configuration.  It is very
;;; useful for figuring out how things work in Emacs.
;;; ------------------------------------------------------------------------

;;; Code:

(use-package helpful
    :commands (helpful-callable helpful-variable helpful-command helpful-key)
    :config
    ;; Note that the built-in `describe-function' includes both functions
    ;; and macros. `helpful-function' is functions only, so we provide
    ;; `helpful-callable' as a drop-in replacement.
    (global-set-key (kbd "C-h f") #'helpful-callable)

    (global-set-key (kbd "C-h v") #'helpful-variable)
    (global-set-key (kbd "C-h k") #'helpful-key)
    (global-set-key (kbd "C-h x") #'helpful-command)

    ;; Lookup the current symbol at point. C-c C-d is a common keybinding
    ;; for this in lisp modes.
    (global-set-key (kbd "C-c C-d") #'helpful-at-point)

    ;; Look up *F*unctions (excludes macros).
    ;;
    ;; By default, C-h F is bound to `Info-goto-emacs-command-node'. Helpful
    ;; already links to the manual, if a function is referenced there.
    (global-set-key (kbd "C-h F") #'helpful-function)
    )




(provide 'init-helpful)
;;; init-helpful.el ends here.
