(in-package #:tui-protocol/widgets)

;;; ------------------------------------------------------------------
;;; The widget protocol. A widget is a CLOS object that is its own model:
;;; WIDGET-UPDATE changes it in place and returns it (callers use the
;;; returned value), a command for the TUI loop, and an EVENT that tells the
;;; parent something happened (submit, select, confirm, …). A parent program
;;; keeps its widgets in its model, forwards messages to the focused one, and
;;; draws each into a TUI:SUB-CANVAS.

(defclass widget ()
  ((width :initarg :width :initform 80 :accessor widget-width
          :documentation "Cells across, as of the last WIDGET-VIEW (or the initarg).")
   (height :initarg :height :initform 24 :accessor widget-height
           :documentation "Rows, as of the last WIDGET-VIEW (or the initarg).")
   (focused :initarg :focused :initform t :accessor widget-focused-p
            :documentation "An unfocused widget ignores key and paste messages
and draws no cursor."))
  (:documentation "Base class of tui-protocol widgets."))

(defgeneric widget-init (widget)
  (:documentation "→ the command a widget needs at start (a spinner's first
tick), or NIL.")
  (:method ((widget widget)) nil))

(defgeneric widget-update (widget message)
  (:documentation "Handle MESSAGE. → (values widget command event): WIDGET
(updated in place), a TUI command or NIL, a WIDGET-EVENT or NIL. Key
messages with action :RELEASE are ignored; :PRESS and :REPEAT act. An
unfocused widget ignores key and paste messages.")
  (:method ((widget widget) message)
    (declare (ignore message))
    (values widget nil nil)))

(defgeneric widget-view (widget canvas)
  (:documentation "Draw WIDGET on CANVAS (its whole area; pass a
TUI:SUB-CANVAS to place it). Records the canvas size as WIDGET-WIDTH /
WIDGET-HEIGHT first, so scrolling uses the size last drawn. The return
value is ignored."))

(defun %ignored-message-p (widget message)
  "True for a key release, and for key and paste messages to an unfocused
WIDGET. Mouse, tick, resize and focus messages always get through."
  (or (and (tui:key-message-p message)
           (eq :release (tui:key-message-action message)))
      (and (not (widget-focused-p widget))
           (or (tui:key-message-p message) (tui:paste-message-p message)))))

(defmethod widget-update :around ((widget widget) message)
  ;; The focus / release rule lives here once; primary methods see live messages.
  (if (%ignored-message-p widget message)
      (values widget nil nil)
      (call-next-method)))

;;; ------------------------------------------------------------------
;;; Events

(defstruct widget-event
  (widget nil))

(defstruct (submit-event (:include widget-event))
  "A TEXT-INPUT's buffer was submitted."
  (text "" :type string))

(defstruct (cancel-event (:include widget-event))
  "Escape: the user backed out (text-input, select-list, confirm-dialog).")

(defstruct (edit-event (:include widget-event))
  "Ctrl+G in a TEXT-INPUT: the parent should open an external editor on TEXT
and put the result back with (SETF TEXT-INPUT-VALUE)."
  (text "" :type string))

(defstruct (select-event (:include widget-event))
  "A SELECT-LIST item was chosen."
  (index 0 :type (integer 0))
  (item nil))

(defstruct (confirm-event (:include widget-event))
  "A CONFIRM-DIALOG choice was made. FEEDBACK is the text typed after the
feedback choice (possibly \"\"), NIL for the other choices."
  (choice nil :type symbol)
  (feedback nil :type (or null string)))

;;; ------------------------------------------------------------------
;;; One widget as a whole program: for tests, demos and one-widget tools.

(defclass widget-program ()
  ((widget :initarg :widget :reader widget-program-widget)
   (quit-on :initarg :quit-on :initform '() :reader widget-program-quit-on
            :documentation "Event types (symbols) that end the program.")
   (events :initform '() :accessor %events
           :documentation "Recorded events, newest first; guarded by LOCK.")
   (lock :initform (bt2:make-lock :name "widget-program-events") :reader %events-lock))
  (:documentation "A tui-protocol program whose model is one widget. TUI-INIT
→ (values widget (widget-init widget)); TUI-UPDATE forwards every message to
WIDGET-UPDATE, records the event and adds :QUIT to the command when the event
is of a QUIT-ON type; TUI-VIEW draws the widget on the whole canvas."))

(defun make-widget-program (widget &key quit-on)
  "A WIDGET-PROGRAM running WIDGET. QUIT-ON is a list of event type symbols."
  (check-type widget widget)
  (check-type quit-on list)
  (make-instance 'widget-program :widget widget :quit-on quit-on))

(defun widget-program-events (program)
  "Events the widget returned so far, oldest first. Safe to call from another
thread while the program runs."
  (bt2:with-lock-held ((%events-lock program))
    (reverse (%events program))))

(defun %quit-event-p (program event)
  (and event
       (some (lambda (type) (typep event type)) (widget-program-quit-on program))))

(defmethod tui:tui-init ((program widget-program))
  (let ((widget (widget-program-widget program)))
    (values widget (widget-init widget))))

(defmethod tui:tui-update ((program widget-program) model message)
  (multiple-value-bind (widget command event) (widget-update model message)
    (when event
      (bt2:with-lock-held ((%events-lock program))
        (push event (%events program))))
    (values widget
            (cond ((not (%quit-event-p program event)) command)
                  (command (tui:batch command :quit))
                  (t :quit)))))

(defmethod tui:tui-view ((program widget-program) model canvas)
  (declare (ignore program))
  (widget-view model canvas))
