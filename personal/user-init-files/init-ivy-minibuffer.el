;;; init-ivy-minibuffer.el -- Configure the package
;;;
;;; Commentary:
;;;
;;; Ivy is an excellent completion framework for Emacs.  It provides a minimal
;;; yet powerful selection menu that appears when you open files, switch
;;; buffers, and for many other tasks in Emacs.  Counsel is a customized set of
;;; commands to replace `find-file` with `counsel-find-file`, etc which provide
;;; useful commands for each of the default completion commands.
;;; ------------------------------------------------------------------------

;;; Code:


;;; ------------------------------------------------------------------------
;;; Swiper and IVY mode
(use-package ivy
   :diminish
   :bind (("C-s" . swiper)
          :map ivy-minibuffer-map
          ;;; ("TAB" . ivy-alt-done)
          ("C-l" . ivy-alt-done)
          ("C-j" . ivy-next-line)
          ("C-k" . ivy-previous-line)
          :map ivy-switch-buffer-map
          ("C-k" . ivy-previous-line)
          ("C-l" . ivy-done)
          ("C-d" . ivy-switch-buffer-kill)
          :map ivy-reverse-i-search-map
          ("C-k" . ivy-previous-line)
          ("C-d" . ivy-reverse-i-search-kill))
   :custom (ivy-use-virtual-buffers t)
   :config
   (ivy-mode 1))

(use-package ivy-rich
   :defer t
   :after ivy
   :init
   (ivy-rich-mode 1))

(use-package ivy-yasnippet
   :defer t)

;;; Swiper is an alternative to isearch that uses Ivy to show an overview of all
;;; matches.

(use-package swiper
   :after ivy)

;;; ~ivy-mode~ ensures that any Emacs command using completing-read-function
;;; uses ivy for completion.  Counsel takes this further, providing versions of
;;; common Emacs commands that are customised to make the best use of Ivy. For
;;; example, ~counsel-find-file~ has some additional keybindings. Pressing =DEL=
;;; will move you to the parent directory.

(use-package counsel
   :diminish
   :defer t
   :bind (("C-M-j" . 'counsel-switch-buffer)
            :map minibuffer-local-map
            ("C-r" . 'counsel-minibuffer-history))
   :custom
   (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
   :config
   (counsel-mode 1))

;;; ~prescient.el~ is a library which sorts and filters lists of candidates,
;;; such as appear when you use a package like =Ivy= or =Company=.

(use-package ivy-prescient
  :after counsel
  :custom
  (ivy-prescient-enable-filtering nil)
  :config
  (prescient-persist-mode 1)
  (ivy-prescient-mode 1))


(provide 'init-ivy-minibuffer)
;;; init-ivy-minibuffer.el ends here.
