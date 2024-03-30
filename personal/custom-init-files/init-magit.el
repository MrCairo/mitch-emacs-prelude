;;; init-magit.el -- Configure the package
;;;
;;; Commentary:
;;; ------------------------------------------------------------------------
;;; [[https://magit.vc/][Magit]] is the best Git interface I've ever used.
;;; Common Git operations are easy to execute quickly using Magit's command
;;; panel system.

;;; Code:

(use-package magit
   :defer t
   :commands (magit-status magit-get-current-branch)
   :custom
   (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
   )

;; NOTE: Make sure to configure a GitHub token before using this package!
;; - https://magit.vc/manual/forge/Token-Creation.html#Token-Creation
;; - https://magit.vc/manual/ghub/Getting-Started.html#Getting-Started

(use-package forge
  :after magit)

(provide 'init-magit)
;;; init-magit.el ends here.
