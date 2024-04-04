;;; init-org.el -- Configure the package
;;;
;;; Commentary:
;;; ------------------------------------------------------------------------
;;; [[https://magit.vc/][Magit]] is the best Git interface I've ever used.
;;; Common Git operations are easy to execute quickly using Magit's command
;;; panel system.

;;; Code:

(eval-when-compile (progn
                       (defvar visual-fill-column-width)
                       (defvar visual-fill-column-center-text)))


(defun mrf/org-set-src-colors ()
    (defface org-block-begin-line
        '((t (:underline "#1D2C39" :foreground "#676E95" :background "#1D2C39")))
        "Face used for the line delimiting the begin of source blocks.")

    (defface org-inline-src-block
        '((t (:background "#32302F")))
        "Face used for the source block background.")

    (defface org-block-end-line
        '((t (:overline "#1D2C39" :foreground "#676E95" :background "#1D2C39")))
        "Face used for the line delimiting the end of source blocks.")
    )

(use-package org
    :defer t
    :commands (org-capture org-agenda)
    :hook (org-mode . mrf/org-mode-setup)
    :config
    (general-def org-mode-map
        "C-c e" 'org-edit-src-code)
    ;; Save Org buffers after refiling!
    (advice-add 'org-refile :after 'org-save-all-org-buffers))

(defun mrf/org-mode-visual-fill ()
    (setq visual-fill-column-width 120
        visual-fill-column-center-text t)
    (visual-fill-column-mode 1))

(use-package visual-fill-column
    :ensure t
    :after (org)
    :hook (org-mode . mrf/org-mode-visual-fill))

;;; -----------------------------------------------------------------

(use-package org-bullets
    :ensure t
    :after (org)
    :hook (org-mode . org-bullets-mode)
    :custom
    (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

;;; -----------------------------------------------------------------

(with-eval-after-load 'org
    (require 'ox-gfm nil t)
    (mrf/org-set-src-colors)
    ;; This is needed as of Org 9.2
    (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
    (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
    (add-to-list 'org-structure-template-alist '("py" . "src python")))

(provide 'init-org)
;;; init-org.el ends here.
