;;; init-quelpa.el -- Configure themes
;;;
;;; Commentary:
;;;
;;; ------------------------------------------------------------------------

;;; Code:

(eval-when-compile (defvar quelpa-upgrade-interval))
(if (use-package quelpa)
   (use-package quelpa
      :defer t
      :hook (after-init . quelpa-upgrade-all-maybe)
      :custom
      quelpa-upgrade-interval 7)
   )

(setq quelpa-upgrade-interval 7)
(add-hook 'after-init-hook #'quelpa-upgrade-all-maybe)

(provide 'init-quelpa)
;;; init-quelpa.el ends here.
