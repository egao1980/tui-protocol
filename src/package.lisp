(defpackage #:tui-protocol
  (:use #:cl)
  (:nicknames #:stack-tui)
  (:local-nicknames (#:tp #:terminal-protocol))
  (:export
   ;; conditions
   #:tui-error #:tui-backend-error #:tui-backend-error-cause
   ;; canvas
   #:canvas #:make-canvas #:canvas-cols #:canvas-rows #:canvas-cell
   #:sub-canvas #:clear-canvas #:draw-text #:fill-rect #:draw-box
   #:canvas-lines
   ;; rendering
   #:render-diff #:render-full
   ;; messages
   #:message #:key-message #:mouse-message #:paste-message #:focus-message
   #:resize-message #:tick-message
   #:key-message-key #:key-message-text #:key-message-modifiers #:key-message-action
   #:mouse-message-button #:mouse-message-action #:mouse-message-x #:mouse-message-y
   #:mouse-message-modifiers
   #:paste-message-text #:focus-message-focused-p
   #:resize-message-cols #:resize-message-rows #:tick-message-time
   #:make-key-message #:make-mouse-message #:make-paste-message #:make-focus-message
   #:make-resize-message #:make-tick-message
   #:key-message-p #:mouse-message-p #:paste-message-p #:focus-message-p
   #:resize-message-p #:tick-message-p
   ;; input
   #:input-decoder #:make-input-decoder #:decode-input #:flush-input #:char-key
   ;; program
   #:tui-init #:tui-update #:tui-view
   #:tick #:batch #:task
   #:tui-backend #:*tui-backend*
   #:backend-start #:backend-stop #:backend-read #:backend-write #:backend-size
   #:run-tui #:tui-send #:*tui*))
