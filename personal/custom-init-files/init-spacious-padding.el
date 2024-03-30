;;; init-spacious-padding.el -- Configure spacious-padding
;;;
;;; Commentary:
;;;
;;; This package provides a global minor mode to increase the spacing/padding of
;;; Emacs windows and frames.  The idea is to make editing and reading feel more
;;; comfortable.  Enable the mode with M-x spacious-padding-mode.  Adjust the
;;; exact spacing values by modifying the user option spacious-padding-widths.
;;; ------------------------------------------------------------------------

;;; Code:
;;; ==========================================================================

(use-package spacious-padding
   :hook (after-init . spacious-padding-mode)
   :custom
   (spacious-padding-widths
      '(:internal-border-width 15
	  :header-line-width 4
	  :mode-line-width 6
	  :tab-width 4
	  :right-divider-width 30
	  :scroll-bar-width 8))
   )

;; Read the doc string of `spacious-padding-subtle-mode-line' as it
;; is very flexible and provides several examples.
;; (setq spacious-padding-subtle-mode-line
;;       `( :mode-line-active 'default
;;          :mode-line-inactive vertical-border))

(provide 'init-spacious-padding)
;;; init-spacious-padding.el ends here.
