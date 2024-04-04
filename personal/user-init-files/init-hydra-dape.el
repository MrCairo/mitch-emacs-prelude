;;; init-hydra-dape.el -- Configure the package
;;;
;;; Commentary:
;;;
;;; ------------------------------------------------------------------------

;;; Code:

;;; DAPE-Hydra

(defun mrf/dape-end-debug-session ()
  "End the debug session and delete project Python buffers."
  (interactive)
  (dape-quit))

(defun mrf/dape-delete-all-debug-sessions ()
  "End the debug session and delete project Python buffers and all breakpoints."
  (interactive)
  (dape-breakpoint-remove-all)
  (mrf/dape-end-debug-session))

(defun mrf/hydra-define-dape ()
  "DAPE commands defined for hydra."
  (defhydra dape-hydra (:color pink :hint nil :foreign-keys run)
            "
  ^Stepping^          ^Switch^                 ^Breakpoints^          ^Debug^                     ^Eval
  ^^^^^^^^----------------------------------------------------------------------------------------------------------------
  _._: Next           _st_: Thread            _bb_: Toggle           _dd_: Debug                 _ee_: Eval Expression
  _/_: Step in        _si_: Info              _bd_: Delete           _dw_: Watch dwim
  _,_: Step out       _sf_: Stack Frame       _ba_: Add              _dx_: end session
  _c_: Continue       _su_: Up stack frame    _bc_: Set condition    _dX_: end all sessions
  _r_: Restart frame  _sd_: Down stack frame  _bl_: Set log message
  _Q_: Disconnect     _sR_: Session Repl
                        _sU_: Info Update

"
            ("n" dape-next)
            ("i" dape-step-in)
            ("o" dape-step-out)
            ("." dape-next)
            ("/" dape-step-in)
            ("," dape-step-out)
            ("c" dape-continue)
            ("r" dape-restart)
            ("si" dape-info)
            ("st" dape-select-thread)
            ("sf" dape-select-stack)
            ("su" dape-stack-select-up)
            ("sU" dape-info-update)
            ("sd" dape-stack-select-down)
            ("sR" dape-repl)
            ("bb" dape-breakpoint-toggle)
            ("ba" dape--breakpoint-place)
            ("bd" dape-breakpoint-remove-at-point)
            ("bc" dape-breakpoint-expression)
            ("bl" dape-breakpoint-log)
            ("dd" dape)
            ("dw" dape-watch-dwim)
            ("ee" dape-evaluate-expression)
            ("dx" mrf/dape-end-debug-session)
            ("dX" mrf/dape-delete-all-debug-sessions)
            ("x" nil "exit Hydra" :color yellow)
            ("q" mrf/dape-end-debug-session "quit" :color blue)
            ("Q" mrf/dape-delete-all-debug-sessions :color red)))

(provide 'init-hydra-dape)

;;; init-hydra-dape.el ends here.
