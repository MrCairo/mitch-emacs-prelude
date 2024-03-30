;;; init-undo.el -- Configure the package
;;;
;;; Commentary:
;;;
;;; Two undo packages which are both pretty good.
;;; Vundo is a very basic but clean undo package.
;;; ------------------------------------------------------------------------

;;; Code:

(eval-when-compile (progn
                      (defvar enable-vundo)
                      (defvar vundo-unicode-symbols)
                      (defvar vundo-glyph-alist)))

(use-package vundo
  :if (equal enable-vundo t)
  :ensure t
  :bind (("C-x u" . vundo)
         ("C-x r u" . vundo))
  :config
  (setq vundo-glyph-alist vundo-unicode-symbols)
  (set-face-attribute 'vundo-default nil :family "Wingdings2"))

;;; -------------------------------------------------------------------------
;;; undo-tree handling

(defun mrf/undo-tree-hook ()
  "Attempt to set the undo-tree frame size."
  (set-frame-width (selected-frame) 20))

(use-package undo-tree
  :if (not (equal enable-vundo t))
  :defer t
  ;; :hook (undo-tree-visualizer-mode-hook . mrf/undo-tree-hook)
  :custom
  (undo-tree-visualizer-timestamps t)
  ;; (undo-tree-visualizer-diff t)
  (undo-tree-enable-undo-in-region t)
  ;; 10X bump of the undo limits to avoid issues with premature
  ;; Emacs GC which truncages the undo history very aggresively
  (undo-limit 800000)
  (undo-strong-limit 12000000)
  (undo-outer-limit 120000000)
  :config
  (global-undo-tree-mode)
  ;; This prevents the *.~undo-tree~ files from being persisted.
  (with-eval-after-load 'undo-tree
    (setq undo-tree-auto-save-history nil)))


(provide 'init-undo)
;;; init-undo.el ends here.
