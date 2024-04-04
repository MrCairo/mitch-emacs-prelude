;;; init-python.el -- Configure the package
;;;
;;; Commentary:
;;; *** IMPORTANT
;;; Before any work can begin in python, make sure that the right packages are
;;; installed.
;;;
;;;  pip3 install --upgrade pip
;;;  pip3 install "python-lsp-server[all]"
;;;  pip3 install debugpy
;;;
;;; In addition to that, it is important that =autopep8=, which is a script, can
;;; reach python or python3. For my configuration, =autopep8= looks for python
;;; this like this:
;;;   #!/Library/Frameworks/Python.framework/Versions/Current/bin/python3
;;;
;;; *Note:* If this python path doesn't exist, you will see the very annoying
;;; *message:
;;;     file-missing "Doing vfork" "No such file or directory"
;;;
;;; ------------------------------------------------------------------------

;;; Code:
;;; This package needs to be loaded _after_ hydra is done.
;;; ------------------------------------------------------------------------

(eval-when-compile (progn
                      (defvar enable-dap)
                      (defvar enable-anaconda)))

(defun mrf/load-python-file-hook ()
   "Hook to run when loading a python file."
   (python-mode)
   (highlight-indentation-mode 0)
   (display-fill-column-indicator-mode t))

(use-package python
    :defer t
    :mode ("\\.py\\'" . mrf/load-python-file-hook)
    :config
    (use-package blacken)
    :interpreter ("python" . python-mode))

(use-package blacken) ;Format Python file upon save.

(if (boundp 'python-shell-completion-native-disabled-interpreters)
   (add-to-list 'python-shell-completion-native-disabled-interpreters "python3")
   (setq python-shell-completion-native-disabled-interpreters '("python3")))

;;; autopep8 automatically formats Python code to conform to the `PEP 8` style
;;; guide.  It uses the pycodestyle_ utility to determine what parts of the code
;;; needs to be formatted.  autopep8 is capable of fixing most of the formatting
;;; issues_ that can be reported by pycodestyle. Refer to the
;;; [[IMPORTANT][IMPORTANT]] section above for possible issues when autopep8 is
;;; installed.
(use-package py-autopep8
    :after (python)
    :hook ((python-mode) . py-autopep8-mode))

; Anaconda support
; ===========================================================================
(if (equal enable-anaconda t)
   (use-package anaconda-mode
      :bind (("C-c C-x" . next-error))
      :config
      (require 'pyvenv)
	(general-define-key
         :keymaps '(python-mode-map)
         "C-c g o"    'anaconda-mode-find-definitions-other-frame
         "C-c g g"    'anaconda-mode-find-definitions)
      :hook
      (python-mode-hook . anaconda-eldoc-mode)))

(provide 'init-python)
;;; init-python.el ends here.
