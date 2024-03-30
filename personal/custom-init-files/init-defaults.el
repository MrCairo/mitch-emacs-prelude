;;; init-defaults.el -- Define / set default variables.
;;;
;;; Commentary:
;;; ------------------------------------------------------------------------

;;; Code:

;;; -------------------------------------------------------------------------
(eval-when-compile (defvar working-files-directory))

(setq-default
   ;; enable smooth scrolling.
   pixel-scroll-mode t
   ;; try to guess target directory
   dired-dwim-target t
   ;; truncate lines even in partial-width windows
   truncate-partial-width-windows 1
   ;; disable auto save
   auto-save-default nil
   ;; disable backup (No ~ tilde files)
   backup-inhibited t
   ;; Refresh buffer if file has changed
   global-auto-revert-mode 1
   global-auto-revert-non-file-buffers t
   ;; Reasonable buffer length
   history-length 25
   ;; Hide the startup message
   inhibit-startup-message t
   ;; emacs lisp tab size
   lisp-indent-offset '3
   ;; Set up the visible bell
   visible-bell t
   ;; long lines of text do not wrap
   truncate-lines 1
   ;; Default line limit for fills
   fill-column 80
   ;; Needed to fix an issue on Mac which causes dired to fail
   dired-listing-switches "-agho --group-directories-first"
   )

(global-display-line-numbers-mode 1) ;; Line numbers appear everywhere
(save-place-mode 1)                  ;; Remember where we were last editing a file.
(savehist-mode t)
(show-paren-mode 1)
(tool-bar-mode -1)                   ;; Hide the toolbar
(global-prettify-symbols-mode 1)     ;; Display pretty symbols (i.e. λ = lambda)

(column-number-mode)
(use-package page-break-lines
   :config
   (global-page-break-lines-mode))

;;; Different emacs configuration installs with have their own configuration
;;; directory.
(make-directory working-files-directory t)  ;; Continues to work even if dir exists

(setq user-emacs-directory working-files-directory)
(message (concat ">>> Setting emacs-working-files directory to: " user-emacs-directory))

(let ((path-from-shell (replace-regexp-in-string "[ \t\n]*$" ""
                          (shell-command-to-string "$SHELL --login -c 'echo $PATH'"))))
   (setenv "PATH" path-from-shell)
   (setq exec-path (split-string path-from-shell path-separator))
   (add-to-list 'exec-path "/opt/homebrew/bin")
   (add-to-list 'exec-path "/usr/local/bin")
   (add-to-list 'exec-path "/opt/homebrew/opt/openjdk/bin")
   (add-to-list 'exec-path "/opt/homebrew/opt/node@20/bin/node")
   (setq-default insert-directory-program "gls"
      dired-use-ls-dired t
      dired-listing-switches "-al --group-directories-first"))

(provide 'init-defaults)
;;; init-defaults.el ends here.
