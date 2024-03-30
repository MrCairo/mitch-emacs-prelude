;;; init-elpy.el -- Configure the Elpy package
;;;
;;; Commentary:
;;; Elpy is an Emacs package to bring powerful Python editing to Emacs.  It
;;; combines and configures a number of other packages, both written in Emacs
;;; Lisp as well as Python.  Elpy is fully documented at
;;; [[https://elpy.readthedocs.io/en/latest/index.html][read the docs]].
;;; ------------------------------------------------------------------------

;;; Code:

;;; ------------------------------------------------------------------------

(eval-when-compile (progn
                      (defvar elpy-modules)
                      (defvar enable-elpy)))

(prelude-require-package 'general)

;;; ------------------------------------------------------------------------
(if (equal enable-elpy t)
   (progn
      (use-package elpy
         :after (python)
         :custom
         (elpy-rpc-python-command "python3")
         (display-fill-column-indicator-mode 1)
         (highlight-indentation-mode 0)
         :config
         (message "elpy loaded")
         (elpy-enable)
         (general-define-key
            :keymaps '(python-mode-map)
            "C-c g a"    'elpy-goto-assignment
            "C-c g o"    'elpy-goto-definition-other-window
            "C-c g g"    'elpy-goto-definition
            "C-c g ?"    'elpy-doc))

      (use-package flycheck
         :ensure t
         :after (elpy)
         :hook (elpy-mode . flycheck-mode)
         :config
         (setq elpy-modules (delq 'elpy-module-flymake elpy-modules)))
      ))

(provide 'init-elpy)
;;; init-elpy.el ends here.
