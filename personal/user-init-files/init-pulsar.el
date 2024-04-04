;;; init-pulsar.el -- Configure the package
;;;
;;; Commentary:
;;; ------------------------------------------------------------------------

;;; Code:

;;; ==========================================================================

(use-package pulsar
   :config
   (pulsar-global-mode)
   (let ((map global-map))
      (define-key map (kbd "C-c h p") #'pulsar-pulse-line)
      (define-key map (kbd "C-c h h") #'pulsar-highlight-line))
   :custom
   (pulsar-pulse t)
   (pulsar-delay 0.055)
   (pulsar-iterations 10)
   (pulsar-face 'pulsar-magenta)
   (pulsar-highlight-face 'pulsar-yellow))

(provide 'init-pulsar)
;;; init-pulsar.el ends here.
