;;; init-general.el -- Configure the package
;;;
;;; Commentary:
;;;
;;; ------------------------------------------------------------------------

;;; Code:

(prelude-require-package 'general)

(use-package general
   :demand t
   :config
   (general-def prog-mode-map
      "C-c ]"  'indent-region
      "C-c }"  'indent-region)
   (general-define-key
      "C-x C-j" 'dired-jump)
   (use-package evil-nerd-commenter
      :bind ("M-/" . evilnc-comment-or-uncomment-lines)))

(provide 'init-general)
;;; init-general.el ends here.
