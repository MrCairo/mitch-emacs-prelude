;;; init-counsel.el -- Configure the package
;;;
;;; Commentary:
;;; ------------------------------------------------------------------------

;;; Code:

;;; ==========================================================================

(use-package counsel
   :bind (("C-M-j" . 'counsel-switch-buffer)
    	    :map minibuffer-local-map
    	    ("C-r" . 'counsel-minibuffer-history))
   :custom
   (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
   :config
   (counsel-mode 1))

(provide 'init-counsel)
;;; init-counsel.el ends here.
