;;; init-misc.el -- Configure the package
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

(defun mrf/set-fill-column-interactively (num)
    (interactive "nColumn: ")
    (set-fill-column num))

(general-define-key
    "C-c i f" 'mrf/insert-buffer-name-at-point
    "C-c i F" 'mrf/insert-buffer-full-name-at-point )

(general-create-definer my-custom-key-prefix
    ;; Create a custom prefix
    :prefix "C-s-m" )

(my-custom-key-prefix
    "S" 'smartparens-strict-mode
    "d" 'dashboard-open
    "|" 'global-display-fill-column-indicator-mode
    "f c" 'mrf/set-fill-column-interactively
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

(global-display-line-numbers-mode 1) ;; Line numbers appear everywhere
(save-place-mode 1)                  ;; Remember where we were last editing a file.
(savehist-mode t)
(show-paren-mode 1)
(tool-bar-mode -1)                   ;; Hide the toolbar
(global-prettify-symbols-mode 1)     ;; Display pretty symbols (i.e. λ = lambda)
(column-number-mode)
(prelude-swap-meta-and-super)

(diminish 'projectile-mode "Proj")

(setq-default initial-scratch-message
    (concat ";; Hello, World and Happy hacking, "
        user-login-name "\n;; Press C-s-o to open the Dashboard\n\n"))

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

(provide 'init-misc)
;;; init-misc.el ends here.
