;;; init-dape.el -- Configure the package
;;;
;;; Commentary:
;;;
;;; ------------------------------------------------------------------------

;;; Code:

;;; Debug Adapter Protocol for Emacs (DAPE)

(eval-when-compile (defvar enable-dap))

(require 'jsonrpc)

(use-package dape
   :defer t
  :after (jsonrpc)
   ;; To use window configuration like gud (gdb-mi)
  ;; :init
  ;; (setq dape-buffer-window-arrangement 'gud)
  :custom
  ;; Info buffers to the right
  (dape-buffer-window-arrangement 'right)
  ;; To not display info and/or buffers on startup
  ;; (remove-hook 'dape-on-start-hooks 'dape-info)
  ;; (remove-hook 'dape-on-start-hooks 'dape-repl)

  ;; To display info and/or repl buffers on stopped
  ;; (add-hook 'dape-on-stopped-hooks 'dape-info)
  ;; (add-hook 'dape-on-stopped-hooks 'dape-repl)

  ;; By default dape uses gdb keybinding prefix
  ;; If you do not want to use any prefix, set it to nil.
  ;; (setq dape-key-prefix "\C-x\C-a")

  ;; Kill compile buffer on build success
  ;; (add-hook 'dape-compile-compile-hooks 'kill-buffer)

  ;; Save buffers on startup, useful for interpreted languages
  ;; (add-hook 'dape-on-start-hooks
  ;;           (defun dape--save-on-start ()
  ;;             (save-some-buffers t t)))

  ;; Projectile users
  (setq dape-cwd-fn 'projectile-project-root)
  ;; :straight (dape :type git
  ;;         :host github :repo "emacs-straight/dape"
  ;;         :files ("*" (:exclude ".git")))
  :config
  (message "DAPE Configured.")
  )

(provide 'init-dape)
;;; init-dape.el ends here.
