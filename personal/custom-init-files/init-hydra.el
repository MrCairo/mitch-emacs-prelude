;;; init-hydra.el -- Configure the package
;;;
;;; Commentary:
;;; ------------------------------------------------------------------------
;;; This is a package for GNU Emacs that can be used to tie related commands
;;; into a family of short bindings with a common prefix - a Hydra.  Once you
;;; summon the Hydra through the prefixed binding (the body + any one head), all
;;; heads can be called in succession with only a short extension.
;;;
;;; The Hydra is vanquished once Hercules, any binding that isn't the Hydra's
;;; head, arrives.  Note that Hercules, besides vanquishing the Hydra, will still
;;; serve his original purpose, calling his proper command.  This makes the Hydra
;;; very seamless, it's like a minor mode that disables itself auto-magically.
;;; ------------------------------------------------------------------------

;;; Code:

(eval-when-compile (defvar enable-dape))

(use-package init-hydra-dape
  :if (equal enable-dape t))

(use-package general)
(use-package hydra
   :config
   (when enable-dape t
      (general-define-key
         :keymaps '(python-mode-map)
         "C-c ."      'dape-hydra/body)
      (mrf/hydra-define-dape)))

(provide 'init-hydra)
;;; init-hydra.el ends here.
