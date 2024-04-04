;;; init-frame-and-fonts.el -- Setup frame/fonts -*-no-byte-compile: t; -*-
;;;
;;; Commentary:
;;; ------------------------------------------------------------------------
;;; Most of this is all defined within functions that can be bound to keys.

;;; Code:

;;; -------------------------------------------------------------------------

;;; -------------------------------------------------------------------------
;;; Frame (view) setup including fonts.
;;; You will most likely need to adjust this font size for your system!

(eval-when-compile (progn
                     (defvar small-mono-font-size)
                     (defvar small-variable-font-size)

                     (defvar medium-mono-font-size)
                     (defvar medium-variable-font-size)

                     (defvar large-mono-font-size)
                     (defvar large-variable-font-size)

                     (defvar x-large-mono-font-size)
                     (defvar x-large-variable-font-size)

                     (defvar default-font-family)
                     (defvar mono-spaced-font-family)
                     (defvar variable-pitch-font-family)

                     (defvar mrf/default-font-size medium-mono-font-size)
                     (defvar mrf/default-variable-font-size
                       medium-variable-font-size)))

;;; ==========================================================================
;; Functions to set the frame size

(defun mrf/frame-recenter (&optional frame)
  "Center FRAME on the screen."
  ;; FRAME can be a frame name, a terminal name,
  ;; or a frame.  If FRAME is omitted or nil, use currently selected frame.
  (interactive)
  ;; (set-frame-size (selected-frame) 250 120)
  (unless (eq 'maximised (frame-parameter nil 'fullscreen))
    (progn
      (let ((width (nth 3 (assq 'geometry (car (display-monitor-attributes-list)))))
            (height (nth 4 (assq 'geometry (car (display-monitor-attributes-list))))))
        (cond (( > width 3000) (mrf/update-large-display))
              (( > width 2000) (mrf/update-built-in-display))
              (t (mrf/set-frame-alpha-maximized)))
        )
      )
    )
  )

(defun mrf/update-large-display ()
  "Update a large display frame."
  (modify-frame-parameters
   frame '((user-position . t)
           (top . 0.0)
           (left . 0.70)
           (width . (text-pixels . 2800))
           (height . (text-pixels . 1650))) ;; 1800
   )
  )

(defun mrf/update-built-in-display ()
  "Update the frame for the built-in display."
  (modify-frame-parameters
   frame '((user-position . t)
           (top . 0.0)
           (left . 0.90)
           (width . (text-pixels . 1800))
           (height . (text-pixels . 1170)));; 1329
   )
  )


;; Set frame transparency
(defun mrf/set-frame-alpha-maximized ()
  "Function to set the alpha and also maximize the frame."
  ;; (set-frame-parameter (selected-frame) 'alpha mrf/frame-transparency)
  (set-frame-parameter (selected-frame) 'fullscreen 'maximized)
  (add-to-list 'default-frame-alist '(fullscreen . maximized)))

;; default window width and height
(defun mrf/custom-set-frame-size ()
  "Simple function to set the default frame width/height."
  ;; (set-frame-parameter (selected-frame) 'alpha mrf/frame-transparency)
  (setq swidth (nth 3 (assq 'geometry (car (display-monitor-attributes-list)))))
  (setq sheight (nth 4 (assq 'geometry (car (display-monitor-attributes-list)))))

  (add-to-list 'default-frame-alist '(fullscreen . maximized))
  (mrf/frame-recenter)
  )

;;; ==========================================================================

;; Default fonts

(defun mrf/update-face-attribute ()
  "Set the font faces."
  ;; ====================================
  (set-face-attribute 'default nil
                      ;; :font "Hack"
                      ;; :font "Fira Code Retina"
                      ;; :font "Menlo"
                      :family default-font-family
                      :height mrf/default-font-size
                      :weight 'medium)

  ;; Set the fixed pitch face
  (set-face-attribute 'fixed-pitch nil
                      ;; :font "Lantinghei TC Demibold"
                      :family mono-spaced-font-family
                      ;; :font "Fira Code Retina"
                      :height mrf/default-font-size
                      :weight 'medium)

  ;; Set the variable pitch face
  (set-face-attribute 'variable-pitch nil
                      :family variable-pitch-font-family
                      :height mrf/default-variable-font-size
                      :weight 'medium))

(mrf/update-face-attribute)
(unless (daemonp)
  (mrf/frame-recenter)
  )


(defvar mrf/font-size-slot 1)

(defun mrf/update-font-size ()
  (message "adjusting font size")
  (cond ((equal mrf/font-size-slot 3)
         (progn
           (message "X-Large Font")
           (setq mrf/default-font-size x-large-mono-font-size
                 mrf/default-variable-font-size x-large-variable-font-size
                 mrf/font-size-slot 2)
           (mrf/update-face-attribute)))
        ((equal mrf/font-size-slot 2)
         (progn
           (message "Large Font")
           (setq mrf/default-font-size large-mono-font-size
                 mrf/default-variable-font-size large-variable-font-size
                 mrf/font-size-slot 1)
           (mrf/update-face-attribute)))
        ((equal mrf/font-size-slot 1)
         (progn
           (message "Medium Font")
           (setq mrf/default-font-size medium-mono-font-size
                 mrf/default-variable-font-size medium-variable-font-size
                 mrf/font-size-slot 0)
           (mrf/update-face-attribute)))
        ((equal mrf/font-size-slot 0)
         (progn
           (message "Small Font")
           (setq mrf/default-font-size small-mono-font-size
                 mrf/default-variable-font-size small-variable-font-size
                 mrf/font-size-slot 3)
           (mrf/update-face-attribute)))
        )
  )


;; Some alternate keys below....
;;(global-set-key (kbd "C-c 1") 'use-small-display-font)
(general-define-key
 "C-c 1" 'use-small-display-font)

(general-define-key
 "C-c 2" 'use-medium-display-font)

(general-define-key
 "C-c 3" 'use-large-display-font)

(general-define-key
 "C-c 4" 'use-x-large-display-font)


(defun mrf/set-frame-font (slot)
  (setq mrf/font-size-slot slot)
  (mrf/update-font-size)
  (mrf/frame-recenter)
  )

(defun use-small-display-font ()
  (interactive)
  (mrf/set-frame-font 0)
  (mrf/frame-recenter)
  )

(defun use-medium-display-font ()
  (interactive)
  (mrf/set-frame-font 1)
  (mrf/frame-recenter)
  )

(defun use-large-display-font ()
  (interactive)
  (mrf/set-frame-font 2)
  (mrf/frame-recenter)
  )

(defun use-x-large-display-font ()
  (interactive)
  (mrf/set-frame-font 3)
  (mrf/frame-recenter)
  )

(add-hook 'after-init-hook 'use-medium-display-font)


(provide 'init-frame-and-fonts)
;;; init-frame-and-fonts.el ends here.
