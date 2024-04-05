;;; user-custom.el -- User-customized packages
;;;
;;; Commentary:
;;;
;;; ------------------------------------------------------------------------

;;; Code:
;;;

(require 'use-package)

(eval-when-compile
    (progn
        (defvar enable-gb-dev)
        (defvar enable-dape)
        (defvar enable-ts)
        (defvar enable-vundo)
        (defvar enable-centaur-tabs)
        (defvar enable-ivy-counsel-swiper)
        (defvar enable-elpy)))

;;
;; When installing from scratch enable this variable to t, otherwise set to
;; nil.
(setq use-package-always-ensure nil)
(setq use-package-always-defer nil)
;;
;; Keep track of the original config (.emacs.d directory).
;; The actual `user-emacs-diectory' will be changed so that customized
;; transient files won't "pollute" the configuration directory.
(defvar emacs-config-directory user-emacs-directory)

(add-to-list 'load-path
   (expand-file-name "personal/user-init-files" emacs-config-directory))

;; -------------------------------------------------------------------------
;; Required first included packages
;;
(use-package init-customize) ;; Must come first. It has all of the variables.
(use-package init-general)
(use-package init-defaults)
(use-package init-auto-package-update)
(use-package init-quelpa)
(use-package dash)

;; -------------------------------------------------------------------------
;; Feature packages
;;
(use-package init-undo)
(use-package init-frame-and-fonts)
(use-package init-ivy-minibuffer
    :if (equal enable-ivy-counsel-swiper t))
(use-package init-theming)
(use-package init-magit)
;; (use-package init-yasnippet)
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
(use-package init-org)
(use-package init-gb-dev
   :if (equal enable-gb-dev t))
(use-package init-helpful)
(use-package init-pulsar)
(use-package init-spacious-padding)
(use-package init-centaur-tabs
   :if (equal enable-centaur-tabs t))
(use-package init-misc)

(provide 'user-custom)
;;; user-custom.el ends here.
