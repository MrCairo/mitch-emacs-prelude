;;; init-customize.el -- Define feature controlling switches
;;;
;;; Commentary:
;;; ------------------------------------------------------------------------
;;; Feature Enabling Switches
;;; Set various variables to =t= to turn on a specific feature or =nil= to
;;; disable it.  Changing any of these values will require a restart of ~emacs~
;;; since these values are inspected only during startup.  *Note:* There are no
;;; thorough dependency check done if any of these values is enabled or
;;; disabled.  There has been some effort, for example, to enable a keymap if
;;; ~dap~ or ~dape~ is enabled.  But if something enabled requires some not
;;; obvious package to be installed, this isn't checked.
;;; ------------------------------------------------------------------------

;;; Code:

;; (defcustom text-mode-hook nil
;;   "Normal hook run when entering Text mode and many related modes."
;;   :type 'hook
;;   :options '(turn-on-auto-fill flyspell-mode)
;;   :group 'wp)

(defgroup mrf-custom nil
  "Customization toggles for Mitch's Emacs installation."
  :group 'Local
  )

(defgroup mrf-custom-fonts nil
   "Customization of fonts and sizes."
  :group 'mrf-custom
  )

(defgroup mrf-custom-theming nil
   "Custom theming values."
   :group 'mrf-custom
   )

(defcustom display-dashboard-at-start t
  "If set to t, the `dashboard' package will be displayed first.
Otherwise, the `dashboard' will be available but in the buffer
*dashboard*."
  :type 'boolean
  :group 'mrf-custom
   )

;;; Feature Switches
(defcustom enable-gb-dev nil
   "If set to t, the z80-mode and other GameBoy related packages
will be enabled."
   :type 'boolean
   :group 'mrf-custom)

(defcustom enable-dape t
  "Set to t to enable the Debug Application Protocol for Emacs."
  :type 'boolean
  :group 'mrf-custom)

(defcustom enable-ts nil
  "Set to t to enable TypeScript handling."
  :type 'boolean
  :group 'mrf-custom)

(defcustom enable-corfu nil
  "Setting to t enables Corfu instead of Ivy.
Corfu is an alternative to the command completion package, IVY which also will
include Swiper and Company.  If this value is set to nil then Ivy is used."
  :type 'boolean
  :group 'mrf-custom)

(defcustom enable-vundo t
  "Set to t to enable `vundo' which is an alternative to Emacs undo.
Setting this value to nil will activate the alternate `undo-tree' package."
  :type 'boolean
  :group 'mrf-custom)

(defcustom enable-centaur-tabs nil
  "Set to t to enable `centaur-tabs' which uses tabs to represent open buffer."
  :type 'boolean
  :group 'mrf-custom)

(defcustom enable-anaconda nil
  "Set to t to enable the `anaconda' Python package.
This OR `enable-elpy' should be set to t but NOT BOTH!"
  :type 'boolean
  :group 'mrf-custom)

(defcustom enable-elpy t
  "Set to t to enable the `elpy' package used for Python development.
This OR `enable-anaconda' should be set to t but NOT BOTH!"
  :type 'boolean
  :group 'mrf-custom)

(defcustom enable-neotree nil
  "Set to t to enable the `neotree' package."
  :type 'boolean
  :group 'mrf-custom)

(defcustom enable-zoom nil
  "Set to t to enable `zoom-mode' - golden-rectangle frame for the active buffer."
  :type 'boolean
  :group 'mrf-custom)

(defcustom custom-docs-dir "~/Documents/Emacs-Related"
   "A directory used to store documents and customized data."
   :type 'string
   :group 'mrf-custom)

(defcustom working-files-directory
   (concat custom-docs-dir
      (concat "/emacs-working-files_" (concat emacs-version "/")))
   "The directory where to store Emacs working files."
   :type 'string
   :group 'mrf-custom)

;;; ------------------------------------------------------------
;;; Theming related

(defcustom theme-list '("palenight-deeper-blue"
                          "ef-symbiosis"
                          "ef-maris-light"
                          "ef-maris-dark"
                          "ef-kassio"
                          "sanityinc-tomorrow-bright"
                          "ef-melissa-dark"
                          "doom-palenight"
                          "deeper-blue")
   "My personal list of themes to cycle through indexed by `theme-selector'."
   :group 'mrf-custom-theming
   :type '(repeat string))

(defcustom theme-selector 0
   "The index into the list of custom themes."
   :group 'mrf-custom-theming
   :type 'natnum)


;;; ------------------------------------------------------------
;;; Font related
(defcustom default-font-family "Hack"
  "The font family used as the default font."
  :type 'string
  :group 'mrf-custom-fonts
  )

(defcustom mono-spaced-font-family "Hack"
  "The font family used as the mono-spaced font."
  :type 'string
  :group 'mrf-custom-fonts
  )

(defcustom variable-pitch-font-family "SF Pro"
  "The font family used as the default proportional font."
  :type 'string
  :group 'mrf-custom-fonts
  )

(defcustom small-mono-font-size 150
  "The small font size in pixels."
  :type 'natnum
  :group 'mrf-custom-fonts
  )

(defcustom medium-mono-font-size 170
  "The medium font size in pixels."
  :type 'natnum
  :group 'mrf-custom-fonts
  )

(defcustom large-mono-font-size 190
  "The large font size in pixels."
  :type 'natnum
  :group 'mrf-custom-fonts
  )

(defcustom x-large-mono-font-size 220
  "The extra-large font size in pixels."
  :type 'natnum
  :group 'mrf-custom-fonts
  )

(defcustom small-variable-font-size 170
  "The small font size in pixels."
  :type 'natnum
  :group 'mrf-custom-fonts
  )

(defcustom medium-variable-font-size 190
  "The small font size in pixels."
  :type 'natnum
  :group 'mrf-custom-fonts
  )

(defcustom large-variable-font-size 210
  "The small font size in pixels."
  :type 'natnum
  :group 'mrf-custom-fonts
  )

(defcustom x-large-variable-font-size 240
  "The small font size in pixels."
  :type 'natnum
  :group 'mrf-custom-fonts
  )

(provide 'init-customize)
;;; init-customize.el ends here.
