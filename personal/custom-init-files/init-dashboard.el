;;; init-dashboard.el -- Configure the package
;;;
;;; Commentary:
;;;
;;; Dashboard is an extensible Emacs startup screen showing you what’s most important.
;;; Value of dashboard-startup-banner can be:
;;;   - nil to display no banner
;;;   - 'official which displays the official emacs logo
;;;   - 'logo which displays an alternative emacs logo
;;;   - 1, 2 or 3 which displays one of the text banners
;;;   - "path/to/your/image.gif", "path/to/your/image.png" or
;;;     "path/to/your/text.txt" which displays whatever gif/image/text you would
;;;     prefer
;;;   - a cons of '("path/to/your/image.png" . "path/to/your/text.txt")

;;; ------------------------------------------------------------------------

;;; Code:
;;; ------------------------------------------------------------------------

(if (display-graphic-p)
    (progn
      (prelude-require-package 'all-the-icons)
      (prelude-require-package 'nerd-icons)
      )
  )

(eval-when-compile (progn
                     (defvar display-dashboard-at-start)
                     (defvar dashboard-footer-messages)
                     (defvar dashboard-banner-logo-title)
                     (defvar dashboard-items)
                     (defvar dashboard-icon-type)
                     (defvar dashboard-display-icons-p)
                     (defvar dashboard-center-content)
                     (defvar dashboard-set-heading-icons)
                     (defvar dashboard-set-file-icons)
                     (defvar dashboard-projects-backend)
                     (defvar dashboard-startup-banner)))

(prelude-require-package 'projectile)

(defun mrf/setup-dashboard-buffer ()
   "Set up the dashboard buffer and optionally make it the first."
   (setq dashboard-items '((recents . 10)
                           (bookmarks . 10)
                           (projects . 10))
      dashboard-icon-type 'all-the-icons
      dashboard-display-icons-p t
      dashboard-center-content t
      dashboard-set-heading-icons t
      dashboard-set-file-icons t
      dashboard-projects-backend 'projectile)

   (global-set-key (kbd "C-s-o") 'dashboard-open)

   (if (equal display-dashboard-at-start t)
      (progn
         (setq initial-buffer-choice (lambda ()
                                        (get-buffer-create "*dashboard*")))
         (dashboard-open))
      (get-buffer-create "*dashboard*")))

(defun mrf/dashboard-banner ()
  "Setup defaults for the dashboard banner buffer."
  (setq dashboard-footer-messages '("Greetings Program!"))
  (setq dashboard-banner-logo-title "Welcome to Emacs!")
  (setq dashboard-startup-banner 'logo))


;;; ------------------------------------------------------------------
;;; Load and configure the Dashboard.
(use-package dashboard
   :init
   (mrf/dashboard-banner)
   :hook ((after-init . mrf/setup-dashboard-buffer)
            (dashboard-mode . mrf/dashboard-banner)))

(provide 'init-dashboard)

;;; init-dashboard.el ends here.
