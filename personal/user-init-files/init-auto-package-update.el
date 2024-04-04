;;; init-auto-package-update.el -- Configure the package
;;;
;;; Commentary:
;;;
;;; ------------------------------------------------------------------------
;;; The auto-package-update package helps us keep our Emacs packages up to date!
;;; It will prompt you after a certain number of days either at startup or at a
;;; specific time of day to remind you to update your packages.
;;;
;;; You can also use =M-x auto-package-update-now= to update right now!

;;; Code:

;;; -------------------------------------------------------------------------
;;; Automatic Package Updates

(use-package auto-package-update
    :ensure t
    :custom
    (auto-package-update-interval 7)
    (auto-package-update-prompt-before-update t)
    (auto-package-update-hide-results t)
    :hook (auto-package-update-after . (lambda ()
                                           (when (package-installed-p 'yasnippet)
                                               (yas-reload-all))))
    :config
    (auto-package-update-maybe)
    (auto-package-update-at-time "09:00"))

(provide 'init-auto-package-update)
;;; init-auto-package-update.el ends here.
