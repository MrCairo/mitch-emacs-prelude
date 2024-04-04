;;; init-gb-dev.el -- Configure the package
;;;
;;; Commentary:
;;; ------------------------------------------------------------------------
;;;
;;; NOTE: These packages must be downloaded from their repo because they
;;;       don't exist in any of the main melpa/elpa/gnu repos.
;;;
;;; RGBDS is a compiler that has been around quite a long time (since 1997).
;;; It supports Z80 and the LR35902 assembler syntaxes that are used in the
;;; development of Game Boy and Game Boy color games.

;;; Code:

;;; ==========================================================================

;;; Download

;;; Load a package via quelpa if z80-mode is not found.
(eval-when-compile (defvar enable-gb-dev))

(when (equal enable-gb-dev t)
   (when (use-package z80-mode)
      (message ">>> Fetching z80-mode")
      (quelpa '(z80-mode :repo "SuperDisk/z80-mode" :fetcher github))
      (use-package z80-mode)
      )

   (when (use-package mwim)
      (quelpa '(mwim :repo "alezost/mwim.el" :fetcher github))
      (use-package mwim)
      )

   (when (use-package rgbds-mode)
      (message ">>> Fetching rgbds-mode")
      (quelpa '(rgbds-mode :repo "MrCairo/rgbds-mode" :fetcher github))
      (use-package rgbds-mode)
      )
   )

(provide 'init-gb-dev)
;;; init-gb-dev.el ends here.
