;;; init-yasnippet.el -- Configure the package
;;;
;;; Commentary:
;;;
;;; These are useful snippets of code that are commonly used in various
;;; languages.  You can even create your own.
;;; ------------------------------------------------------------------------

;;; Code:

;;; -------------------------------------------------------------------------
;;; YASnippets

(eval-when-compile (defvar custom-docs-dir))

(use-package yasnippet
    :defer t
    :config
    (use-package yasnippet-snippets
        :after (yasnippet))
    (yas-global-mode t)
    (define-key yas-minor-mode-map (kbd "<tab>") nil)
    (define-key yas-minor-mode-map (kbd "C-'") #'yas-expand)
    (add-to-list #'yas-snippet-dirs (concat custom-docs-dir "/Snippets"))
    ;; (yas-reload-all)
    (setq yas-prompt-functions '(yas-ido-prompt))
    (defun help/yas-after-exit-snippet-hook-fn ()
        (prettify-symbols-mode))
    (add-hook 'yas-after-exit-snippet-hook #'help/yas-after-exit-snippet-hook-fn)
    :diminish yas-minor-mode)

(provide 'init-yasnippet)
;;; init-yasnippet.el ends here.
