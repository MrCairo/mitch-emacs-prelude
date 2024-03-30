;;; init-local.el -- Configure the package
;;;
;;; Commentary:
;;;
;;; ------------------------------------------------------------------------

;;; Code:
;;; ------------------------------------------------------------------------
;;; Functions to insert the buffer file name at the current cursor position
;;;

(defun mrf/insert-buffer-full-name-at-point ()
  "Insert the buffer name including the file path."
  (interactive)
  (insert buffer-file-name))

(defun mrf/insert-buffer-name-at-point ()
  "Insert the buffer name only."
  (interactive)
  (insert (file-name-nondirectory (buffer-file-name))))

(general-define-key
 "C-c i f" 'mrf/insert-buffer-name-at-point
 "C-c i F" 'mrf/insert-buffer-full-name-at-point
 )

;;
;; Ctl-mouse to adjust/scale fonts will be disabled.
;; I personally like this since it was all to easy to accidentally
;; change the size of the font.
;;
(global-unset-key (kbd "C-<mouse-4>"))
(global-unset-key (kbd "C-<mouse-5>"))
(global-unset-key (kbd "C-<wheel-down>"))
(global-unset-key (kbd "C-<wheel-up>"))

;;; ===========================================================================
;; This is a helpful macro that is used to put double quotes around a word.
(defalias 'quote-word
   (kmacro "\" M-d \" <left> C-y"))

(defalias 'quote-region
   (kmacro "C-w \" \" <left> C-y <right>"))

(general-define-key
   :keymaps '(python-mode-map)
   "C-c C-q"    'quote-region
   "C-c q"      'quote-word
   "C-c |"      'display-fill-column-indicator-mode)

(global-prettify-symbols-mode 1)

(prelude-swap-meta-and-super)

;;; ------------------------------------------------------------------------
;; The following is a list of major mode-hooks variables that are set so that
;; they don't follow the normal global line number ;; mode. If there is any
;; mode that doesn't appear here, more than likely it will have line numbers
;; added. Just add the hook name here to make it so that major mode not have
;; line numbers. This doesn't effect minor modes.

;; Line #'s appear everywhere
;; ... except for when in these modes
(dolist (mode '(dashboard-mode-hook
                  eshell-mode-hook
                  eww-mode-hook
                  help-mode-hook
                  org-mode-hook
                  shell-mode-hook
                  term-mode-hook
                  vterm-mode-hook
                  treemacs-mode-hook
                  vterm-mode-hook))
   (add-hook mode (lambda () (display-line-numbers-mode 0))))

(provide 'init-local)
;;; init-local.el ends here.
