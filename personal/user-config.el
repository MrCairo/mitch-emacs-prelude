;;; user-custom.el -- User-customized packages
;;;
;;; Commentary:
;;;
;;; ------------------------------------------------------------------------

;;; Code:
;;;

(eval-when-compile (progn
                      (defvar enable-gb-dev)
                      (defvar enable-dape)
                      (defvar enable-ts)
                      (defvar enable-vundo)
                      (defvar enable-centaur-tabs)
                      (defvar enable-elpy)
                      ))

(defvar emacs-config-directory user-emacs-directory)

(add-to-list 'load-path (expand-file-name "personal/custom-init-files" emacs-config-directory))

(setq use-package-always-ensure nil)  ;; Only until all packages are installed

(use-package init-customize) ;; Must come first. It has all of the variables.
(use-package init-general)
(use-package init-defaults)
(use-package init-quelpa)
(use-package init-undo)
(use-package init-frame-and-fonts)
(use-package init-minibuffer)
(use-package init-theming)
(use-package init-magit)
;;(use-package init-yasnippet)
(use-package init-dashboard)
(use-package init-dired)
(use-package init-lsp)
(use-package init-dape
   :if (equal enable-dape t))
(use-package init-hydra)
(use-package init-treemacs)
(use-package init-python)
(use-package init-elpy
   :if (equal enable-elpy t))
(use-package init-pyvenv)
(use-package init-terminals)

;; Lastly...
(use-package init-gb-dev
   :if (equal enable-gb-dev t))
(use-package init-helpful)
(use-package init-pulsar)
(use-package init-spacious-padding)
(use-package init-centaur-tabs
   :if (equal enable-centaur-tabs t))
(use-package init-local)

(provide 'user-custom)
;;; user-custom.el ends here.
