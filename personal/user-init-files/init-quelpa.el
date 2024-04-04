;;; init-quelpa.el -- Configure themes
;;;
;;; Commentary:
;;;
;;; ------------------------------------------------------------------------

;;; Code:

(eval-when-compile (defvar quelpa-upgrade-interval))

(use-package quelpa
    :defer t
    :hook (after-init . quelpa-upgrade-all-maybe)
    :custom
    quelpa-upgrade-interval 7)

(provide 'init-quelpa)
;;; init-quelpa.el ends here.
