;;; init-centaur-tabs.el -- Configure the package
;;;
;;; Commentary:
;;; ------------------------------------------------------------------------

;;; Code:

;;; ==========================================================================

(eval-when-compile (progn
		      (defvar enable-centaur-tabs)
		      (defvar centaur-tabs-style)
		      (defvar centaur-tabs-set-icons))
   )

(use-package centaur-tabs
   :if (equal enable-centaur-tabs t)
   :init
   ;; Set the style to rounded with icons (setq centaur-tabs-style "bar")
   (setq centaur-tabs-style "bar")
   (setq centaur-tabs-set-icons t)
   (setq centaur-tabs-set-modified-marker t)
   :bind (("C-c <" . centaur-tabs-backward)
            ("C-c >" . centaur-tabs-forward))
   :config ;; Enable centaur-tabs
   (centaur-tabs-mode t))

(provide 'init-centaur-tabs)
;;; init-centaur-tabs.el ends here.
