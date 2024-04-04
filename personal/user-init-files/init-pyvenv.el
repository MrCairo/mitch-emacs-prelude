;;; init-pyvenv.el -- Configure the package
;;;
;;; Commentary:
;;; We use Pyvenv-auto is a package that automatically changes to the Python
;;; virtual environment based upon the project's directory.  pyvenv-auto looks
;;; at the root director of the project for a =.venv= or =venv= (and a few
;;; others)
;;; ------------------------------------------------------------------------
;;; Code:
;;; ------------------------------------------------------------------------
(use-package pyvenv-auto
   :config (message "Starting pyvenv-auto")
   :hook ((python-mode . pyvenv-auto-run)))

(provide 'init-pyvenv)
;;; init-pyvenv.el ends here.
