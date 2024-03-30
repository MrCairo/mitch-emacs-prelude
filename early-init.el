;;; early-init.el -- Early initialization file for emacs
;;
;; Uncomment when using 'straight' package
;; (setq package-enable-at-startup nil)

;;; Commentary:
;;; =========================================================================
;;; startup
;;; The default is 800 kilobytes.  Measured in bytes.
;;; Process performance tuning
;;
;; ** See early-init.el where some of the allocation variables are set **
;;
;;; Code:

(add-hook 'before-init-hook
   (lambda ()
      ;; warn when opening files bigger than 100MB
      (setq large-file-warning-threshold 100000000)
      ;; reduce the frequency of garbage collection by making it happen on
      ;; each 50MB of allocated data (the default is on every 0.76MB)
      (setq gc-cons-threshold 50000000)))

(setq read-process-output-max (* 4 1024 1024))
(setq process-adaptive-read-buffering nil)


;;; use-package settings.
(setq-default use-package-compute-statistics t
   use-package-verbose t
   use-package-always-ensure nil)

(defun mrf/display-startup-time ()
   "Calculate and display startup time."
   (message "Emacs loaded in %s with %d garbage collections."
      (format "%.2f seconds"
         (float-time
          (time-subtract after-init-time before-init-time)))
        gcs-done))

(add-hook 'emacs-startup-hook #'mrf/display-startup-time)

;;; early-init.el ends here.
