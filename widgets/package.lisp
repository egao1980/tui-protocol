(defpackage #:tui-protocol/widgets
  (:use #:cl)
  (:nicknames #:stack-tui-widgets)
  (:local-nicknames (#:tui #:tui-protocol) (#:tp #:terminal-protocol))
  (:export
   ;; styled text runs
   #:text-run #:text-run-p #:text-run-text #:text-run-style
   #:as-runs #:runs-string #:runs-width #:draw-runs #:wrap-runs #:truncate-runs
   ;; widget protocol
   #:widget #:widget-width #:widget-height #:widget-focused-p
   #:widget-init #:widget-update #:widget-view
   ;; events a widget returns from WIDGET-UPDATE
   #:widget-event #:widget-event-p #:widget-event-widget
   #:submit-event #:submit-event-p #:make-submit-event #:submit-event-text
   #:cancel-event #:cancel-event-p #:make-cancel-event
   #:edit-event #:edit-event-p #:make-edit-event #:edit-event-text
   #:select-event #:select-event-p #:make-select-event
   #:select-event-index #:select-event-item
   #:confirm-event #:confirm-event-p #:make-confirm-event
   #:confirm-event-choice #:confirm-event-feedback
   ;; running one widget as a program
   #:widget-program #:make-widget-program #:widget-program-widget
   #:widget-program-quit-on #:widget-program-events
   ;; text-input
   #:text-input #:text-input-value #:text-input-lines #:text-input-cursor
   #:text-input-cursor-column #:text-input-prompt #:text-input-placeholder
   #:text-input-history #:text-input-insert
   #:text-input-completion-function #:text-input-completion-triggers
   #:text-input-completion-max-items #:text-input-completing-p
   #:text-input-completion-candidates #:text-input-completion-index
   ;; viewport
   #:viewport #:viewport-lines #:viewport-offset #:viewport-follow-p #:viewport-wrap-p
   #:viewport-append #:viewport-append-text #:viewport-row-count
   #:viewport-max-offset #:viewport-at-bottom-p
   ;; select-list
   #:select-list #:select-list-items #:select-list-selected #:select-list-selected-item
   #:select-list-label-function
   ;; confirm-dialog
   #:confirm-dialog #:confirm-dialog-title #:confirm-dialog-message
   #:confirm-dialog-choices #:confirm-dialog-selected #:confirm-dialog-mode
   #:confirm-dialog-feedback-choice #:confirm-dialog-feedback-input
   #:confirm-dialog-feedback-prompt #:+permission-choices+
   ;; spinner
   #:spinner #:spinner-frames #:spinner-frame #:spinner-interval #:spinner-label
   #:spinner-active-p #:spinner-start #:spinner-stop #:+braille-frames+
   ;; status-line
   #:status-line #:status-line-left #:status-line-right #:status-line-style))
