;;; init-theming.el -- Configure themes
;;;
;;; Commentary:
;;;
;;; ------------------------------------------------------------------------

;;; Code:

  ;;; -------------------------------------------------------------------------
(eval-when-compile (progn
                      (defvar custom-docs-dir)
                      (defvar theme-list)
                      (defvar theme-selector)
                      (defvar emacs-config-directory)))

(require 'savehist)
(prelude-require-package 'general)

(let ((docs-dir (concat custom-docs-dir "/Themes/")))
  (add-to-list 'custom-theme-load-path docs-dir))

(use-package ef-themes)
(use-package modus-themes)
(use-package material-theme)
(use-package color-theme-sanityinc-tomorrow )
(use-package darktooth-theme :ensure t)

;; (use-package color-theme-modern
;;    :defer t)
;; (use-package zenburn-theme
;;    :defer t)
;; (use-package doom-themes
;;    :defer t)
;; (use-package kaolin-themes
;;    :defer t)
;; (use-package timu-caribbean-theme)

;; Select a theme (or themes) to load.
;; The last one specified is the one that is used as the current theme.

;; Doom Themes
;; (use-package doom-themes)

(defvar loaded-theme nil
 "The text representation of the loaded custom theme.")

;; Modus (and other protesilaos) Themes

(defun mrf/customize-modus-theme ()
   "Apply modus theme customizations."
   (message "Applying modus customization")
   (setq modus-themes-common-palette-overrides
      '((bg-mode-line-active bg-blue-intense)
          (fg-mode-line-active fg-main)
          (border-mode-line-active blue-intense))))

(add-hook 'after-init-hook 'mrf/customize-modus-theme)

(setq ef-themes-common-palette-overrides
   '(  (bg-mode-line bg-blue-intense)
       (fg-mode-line fg-main)
       (border-mode-line-active blue-intense)))

;; (load-theme 'ef-symbiosis :no-confirm)

;; Custom Theme List and Selection
;; This bit of code contains a list of themes that I like personally and then
;; allows them to be switched between themselves. The index of ~theme-selector~
;; is what is set in order to access a theme via the
;; ~mrf/load-theme-from-selector()~ function.

;;; ==========================================================================

;;
;; 1. The function `mrf/load-theme-from-selector' is called from the
;;    "C-= =" Keybinding (just search for it).
;;
;; 2. Once the new theme is loaded via the `theme-selector', the previous
;;    theme is unloaded (or disabled) the function(s) defined in the
;;    `disable-theme-functions' hook are called (defined in the load-theme.el
;;    package).
;;
;; 3. The function `mrf/cycle-theme-selector' is called by the hook. This
;;    function increments the theme-selector by 1, cycling the value to 0
;;    if beyond the `theme-list' bounds.
;;
(setq-default loaded-theme (nth theme-selector theme-list))
(add-to-list 'savehist-additional-variables 'loaded-theme)
(add-to-list 'savehist-additional-variables 'theme-selector)

  ;;; ==========================================================================

(defun mrf/cycle-theme-selector (&rest theme)
   "Cycle the `theme-selector' by 1, resetting to 0 if beyond array bounds."
   (interactive)
   (message (format ">>> Current theme %S" theme))
   (unless (equal (format "%S" theme) "(user)")
      (if (>= theme-selector (- (length theme-list) 1))
         (setq theme-selector 0)
         (setq-default theme-selector (+ 1 theme-selector)))
      ))

;; This is used to trigger the cycling of the theme-selector
;; It is called when a theme is disabled. The theme is disabled from the
;; `mrf/load-theme-from-selector' function.
(add-hook 'disable-theme-functions #'mrf/cycle-theme-selector)

;; This function simply loads the theme from the theme-list indexed by the
;; ~theme-selector~ variable. Note the advice for ~load-theme~ that deactivates
;; the current theme before activating the new theme. This is done to reset all
;; the colors, a clean slate, before the new theme is activated.

(defun mrf/load-theme-from-selector ()
   "Load the theme in `theme-list' indexed by `theme-selector'."
   (interactive)
   (when loaded-theme
      (disable-theme (intern loaded-theme)))
   (setq loaded-theme (nth theme-selector theme-list))
   (message (concat ">>> Loading theme "
               (format "%d: %S" theme-selector loaded-theme)))
   (load-theme (intern loaded-theme) t)
   (if (equal (fboundp 'mrf/org-font-setup) t)
      (mrf/org-font-setup))
   )

;; Function and code to cycle through some selcted themes.
  ;;; ==========================================================================

(defun mrf/print-custom-theme-name ()
   "Print the current loaded theme from the `theme-list' on the modeline."
   (interactive)
   (message (format "Custom theme is %S" loaded-theme)))

(general-define-key
   "C-c C-=" 'mrf/load-theme-from-selector
   "C-c C-?" 'mrf/print-custom-theme-name)

;; This includes the theme to use in both graphical and non-graphical.

  ;;; ==========================================================================
;;
(if loaded-theme
   (load-theme (intern loaded-theme) t)
  (mrf/load-theme-from-selector))

;; For terminal mode we choose Material theme First
(unless (display-graphic-p)
   (load-theme 'material t))

(provide 'init-theming)
;;; init-theming.el ends here.
