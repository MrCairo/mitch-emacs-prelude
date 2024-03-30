;;; init-terminals.el -- Configure the package
;;;
;;; Commentary:
;;; Various terminals.
;;; ------------------------------------------------------------------------

;;; Code:
;;;
;;; =term-mode= is a built-in terminal emulator in Emacs.  Because it is written
;;; in Emacs Lisp, you can start using it immediately with very little
;;; configuration.  If you are on Linux or macOS, =term-mode= is a great choice
;;; to get started because it supports fairly complex terminal applications
;;; (=htop=, =vim=, etc) and works pretty reliably.  However, because it is
;;; written in Emacs Lisp, it can be slower than other options like =vterm=.
;;; The speed will only be an issue if you regularly run console apps with a lot
;;; of output.
;;;
;;; One important thing to understand is =line-mode= versus =char-mode=.
;;; =line-mode= enables you to use normal Emacs keybindings while moving around
;;; in the terminal buffer while =char-mode= sends most of your keypresses to
;;; the underlying terminal.  While using =term-mode=, you will want to be in
;;; =char-mode= for any terminal applications that have their own keybindings.
;;; If you're just in your usual shell, =line-mode= is sufficient and feels more
;;; integrated with Emacs.
;;;
;;; With =evil-collection= installed, you will automatically switch to
;;; =char-mode= when you enter Evil's insert mode (press =i=).  You will
;;; automatically be switched back to =line-mode= when you enter Evil's normal
;;; mode (press =ESC=).
;;;
;;; Run a terminal with =M-x term!=
;;;
;;; *Useful key bindings:*
;;;   - =C-c C-p= / =C-c C-n= - go back and forward in the buffer's prompts
;;;     (also =[[= and =]]= with evil-mode)
;;;   - =C-c C-k= - Enter char-mode
;;;   - =C-c C-j= - Return to line-mode
;;;   - If you have =evil-collection= installed, =term-mode= will enter char
;;;     mode when you use Evil's Insert mode
;;;

(use-package term
   :commands term
   :config
   (setq explicit-shell-file-name "bash") ;; Change this to zsh, etc
   ;;(setq explicit-zsh-args '())         ;; Use 'explicit-<shell>-args for shell-specific args

   ;; Match the default Bash shell prompt.  Update this if you have a custom prompt
   (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *"))

;;; Better term-mode colors
;;;
;;; The =eterm-256color= package enhances the output of =term-mode= to enable
;;; handling of a wider range of color codes so that many popular terminal
;;; applications look as you would expect them to.  Keep in mind that this
;;; package requires =ncurses= to be installed on your machine so that it has
;;; access to the =tic= program.  Most Linux distributions come with this
;;; program installed already so you may not have to do anything extra to use
;;; it.

;;; ------------------------------------------------------------------------
(use-package eterm-256color
   :hook (term-mode . eterm-256color-mode))

;;; vterm
;;;
;;; [[https://github.com/akermu/emacs-libvterm/][vterm]] is an improved terminal
;;; emulator package which uses a compiled native module to interact with the
;;; underlying terminal applications.  This enables it to be much faster than
;;; =term-mode= and to also provide a more complete terminal emulation
;;; experience.  Make sure that you have the
;;; [[https://github.com/akermu/emacs-libvterm/#requirements][necessary
;;; dependencies]] installed before trying to use =vterm= because there is a
;;; module that will need to be compiled before you can use it successfully.

;;; ------------------------------------------------------------------------
(use-package vterm
   :commands vterm
   :config
   (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")  ;; Set this to match your custom shell prompt
   ;;(setq vterm-shell "zsh")                       ;; Set this to customize the shell to launch
   (setq vterm-max-scrollback 10000))

;;; *** shell-mode
;;;
;;; [[https://www.gnu.org/software/emacs/manual/html_node/emacs/Interactive-Shell.html#Interactive-Shell][shell-mode]]
;;; is a middle ground between =term-mode= and Eshell.  It is *not* a terminal
;;; emulator so more complex terminal programs will not run inside of it.  It
;;; does have much better integration with Emacs because all command input in
;;; this mode is handled by Emacs and then sent to the underlying shell once you
;;; press Enter.  This means that you can use =evil-mode='s editing motions on
;;; the command line, unlike in the terminal emulator modes above.
;;;
;;; *Useful key bindings:*
;;;   - =C-c C-p= / =C-c C-n= - go back and forward in the buffer's prompts (also =[[= and =]]= with evil-mode)
;;;   - =M-p= / =M-n= - go back and forward in the input history
;;;   - =C-c C-u= - delete the current input string backwards up to the cursor
;;;   - =counsel-shell-history= - A searchable history of commands typed into the shell

(provide 'init-terminals)
;;; init-terminals.el ends here.
