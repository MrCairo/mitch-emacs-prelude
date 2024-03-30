;;; init-dired.el -- Configure the package
;;;
;;; Commentary:
;;; ------------------------------------------------------------------------
;;; Dired is a built-in file manager for Emacs that does some pretty amazing
;;; things!  Here are some key bindings you should try out:

;;; *Emacs* / *Evil*
;;; - =n= / =j= - next line
;;; - =p= / =k= - previous line
;;; - =j= / =J= - jump to file in buffer
;;; - =RET= - select file or directory
;;; - =^= - go to parent directory
;;; - =S-RET= / =g O= - Open file in "other" window
;;; - =M-RET= - Show file in other window without focusing (previewing files)
;;; - =g o= (=dired-view-file=) - Open file but in a "preview" mode, close with =q=
;;; - =g= / =g r= Refresh the buffer with =revert-buffer= after changing configuration (and after filesystem changes!)

;;; **** Marking files

;;; - =m= - Marks a file
;;; - =u= - Unmarks a file
;;; - =U= - Unmarks all files in buffer
;;; - =* t= / =t= - Inverts marked files in buffer
;;; - =% m= - Mark files in buffer using regular expression
;;; - =*= - Lots of other auto-marking functions
;;; - =k= / =K= - "Kill" marked items (refresh buffer with =g= / =g r= to get them back)
;;; - Many operations can be done on a single file if there are no active marks!

;;; **** Copying and Renaming files

;;; - =C= - Copy marked files (or if no files are marked, the current file)
;;; - Copying single and multiple files
;;; - =U= - Unmark all files in buffer
;;; - =R= - Rename marked files, renaming multiple is a move!
;;; - =% R= - Rename based on regular expression: =^test= , =old-\&=

;;; *Power command*:
;;; - =C-x C-q= (=dired-toggle-read-only=) - Makes all file names in the buffer
;;;   editable directly to rename them!  Press =Z Z= to confirm renaming or =Z
;;;   Q= to abort.

;;; **** Deleting files

;;; - =D= - Delete marked file
;;; - =d= - Mark file for deletion
;;; - =x= - Execute deletion for marks
;;; - =delete-by-moving-to-trash= - Move to trash instead of deleting permanently

;;; **** Creating and extracting archives

;;; - =Z= - Compress or uncompress a file or folder to (=.tar.gz=)
;;; - =c= - Compress selection to a specific file
;;; - =dired-compress-files-alist= - Bind compression commands to file extension

;;; **** Other common operations

;;; - =T= - Touch (change timestamp)
;;; - =M= - Change file mode
;;; - =O= - Change file owner
;;; - =G= - Change file group
;;; - =S= - Create a symbolic link to this file
;;; - =L= - Load an Emacs Lisp file into Emacs

;;; Code:

(let ((gls (executable-find "gls")))
  (when gls (setq insert-directory-program gls)))

(use-package all-the-icons-dired
   :after (dired)
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired-open
   :after (dired)
   :commands (dired dired-jump)
   :config
   ;; Doesn't work as expected!
   ;;(add-to-list 'dired-open-functions #'dired-open-xdg t)
   (setq dired-open-extensions '(("png" . "feh")
                                   ("mkv" . "mpv"))))

(use-package dired-hide-dotfiles
   :after (dired)
   :hook (dired-mode . dired-hide-dotfiles-mode))
   ;; :config
   ;; (evil-collection-define-key 'normal 'dired-mode-map
   ;;   "H" 'dired-hide-dotfiles-mode))

;;; ------------------------------------------------------------------------
;;; *** Single Window
;;; Dired, by default, opens up multiple windows - one for each directory. It
;;; would be nice to be able to limit =dired= to use just a single
;;; window. [[https://codeberg.org/amano.kenji/dired-single][dired-single]] does
;;; just that. We configure =dired-single= to open up a directory while in dired
;;; with the =C-<return>= key combination. This will then open up the directory
;;; in the buffer named =*dired*=. Whenever a directory is opened with the
;;; =C-<return>= key sequence, that directory will then replace what's currently
;;; in the =*dired*= buffer.

(defun mrf/dired-single-keymap-init ()
   "Bunch of stuff to run for Dired."
   (define-key dired-mode-map
      [remap dired-find-file] 'dired-single-buffer)
   (define-key dired-mode-map
      [remap dired-mouse-find-file-other-window] 'dired-single-buffer-mouse)
   (define-key dired-mode-map
      [remap dired-up-directory] 'dired-single-up-directory))

(use-package dired-single
   :config
   (mrf/dired-single-keymap-init))

(provide 'init-dired)
;;; init-dired.el ends here.
