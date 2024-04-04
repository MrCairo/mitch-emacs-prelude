;;; init-lsp.el -- Configure Language Server Protocol support
;;;
;;; Commentary:
;;;
;;; ------------------------------------------------------------------------

;;; Code:

(defun mrf/lsp-mode-setup ()
   "Setup the LSP Headerline."
   message("Set up LSP header-line.")
   (use-package lsp-headerline)
   (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
   (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
   :demand t
   :commands (lsp lsp-deferred)
   :hook (lsp-mode . mrf/lsp-mode-setup)
   :init
   (setq lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'
   :config
   (lsp-enable-which-key-integration t))

(use-package lsp-ui
   :config (setq lsp-ui-sideline-enable t
              lsp-ui-sideline-show-hover t
              lsp-ui-sideline-delay 0.5
              lsp-ui-sideline-ignore-duplicate t
              lsp-ui-doc-delay 3
              lsp-ui-doc-position 'top
              lsp-ui-doc-alignment 'frame
              lsp-ui-doc-header nil
              lsp-ui-doc-show-with-cursor t
              lsp-ui-doc-include-signature t
              lsp-ui-doc-use-childframe t)
   :commands lsp-ui-mode
   :custom
   (lsp-ui-doc-position 'bottom)
   :hook (lsp-mode . lsp-ui-mode))

(general-def lsp-ui-mode-map
   "C-c l d" 'lsp-ui-doc-focus-frame)

(use-package lsp-treemacs
   :after lsp
   :config
   (lsp-treemacs-sync-mode 1)
   (general-def prog-mode-map
      "C-c t" 'treemacs))

;; (use-package lsp-ivy
;;    :after lsp ivy)

;; Make sure that we set the read buffer above the default 4k
(setq read-process-output-max (* 1024 1024))

(provide 'init-lsp)
;;; init-lsp.el ends here.
