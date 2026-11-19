(in-package #:tui-protocol/widgets)

;;; ------------------------------------------------------------------
;;; select-list
;;;
;;; Keys (focused): Up / Down move the selection by one (clamped, no wrap),
;;; Home / End to the first / last item, Page Up / Page Down by
;;; WIDGET-HEIGHT (clamped), Enter → SELECT-EVENT (index, item) unless the
;;; list is empty, Escape → CANCEL-EVENT.
;;;
;;; View: one item per row from row 0: "> " + label for the selected item
;;; (bold), "  " + label for the others, labels cut with TRUNCATE-RUNS to
;;; W - 2. Rows [top, top + H) with
;;; top = clamp(selected - H + 1, 0, max(0, items - H)). An empty list
;;; draws nothing.

(defclass select-list (widget)
  ((items :initarg :items :initform '())
   (label-function :initarg :label-function :initform #'princ-to-string
                   :accessor select-list-label-function
                   :documentation "Item → string or runs.")
   (initial-selected :initarg :selected :initform 0))
  (:documentation "A vertical list with one selected item."))

(defun select-list-items (list)
  (declare (ignore list))
  (%todo 'select-list-items))

(defun (setf select-list-items) (items list)
  "Replace the items; the selection is clamped to the new list (0 when empty)."
  (declare (ignore items list))
  (%todo '(setf select-list-items)))

(defun select-list-selected (list)
  "Index of the selected item (0 for an empty list)."
  (declare (ignore list))
  (%todo 'select-list-selected))

(defun (setf select-list-selected) (index list)
  "Select INDEX, clamped to the items."
  (declare (ignore index list))
  (%todo '(setf select-list-selected)))

(defun select-list-selected-item (list)
  "The selected item, NIL for an empty list."
  (declare (ignore list))
  (%todo 'select-list-selected-item))

(defmethod widget-update ((list select-list) message)
  (declare (ignore message))
  (%todo 'widget-update))

(defmethod widget-view ((list select-list) canvas)
  (declare (ignore canvas))
  (%todo 'widget-view))

;;; ------------------------------------------------------------------
;;; confirm-dialog: a permission prompt (allow once / always / deny with
;;; feedback).
;;;
;;; CHOICES is a list of (keyword label). MODE is :CHOOSE or :FEEDBACK.
;;; :CHOOSE keys: Up / Down move (clamped), Enter chooses the selection,
;;; digit N (1-9) selects and chooses choice N when it exists, Escape →
;;; CANCEL-EVENT. Choosing FEEDBACK-CHOICE switches to :FEEDBACK (no event);
;;; any other choice → CONFIRM-EVENT (choice, feedback NIL).
;;; :FEEDBACK: messages go to FEEDBACK-INPUT (a TEXT-INPUT, prompt "> ")
;;; except Enter (no modifiers), which → CONFIRM-EVENT (FEEDBACK-CHOICE,
;;; the input's value, possibly ""), and Escape, which returns to :CHOOSE
;;; keeping the typed text.
;;;
;;; View on W x H: TUI:DRAW-BOX over the whole canvas with TITLE. Inside,
;;; from column 2 and IW = W - 4 cells wide: MESSAGE word-wrapped at IW from
;;; row 1; one blank row; one row per choice, "> " (selected, bold) or "  "
;;; then "N. label". In :FEEDBACK, then FEEDBACK-PROMPT (truncated to IW)
;;; and the input on the next row (a 1-row sub-canvas at column 2, IW wide).
;;; Rows past H - 2 are not drawn.

(defparameter +permission-choices+
  '((:allow-once "Allow once") (:allow-always "Always allow") (:deny "Deny")))

(defclass confirm-dialog (widget)
  ((title :initarg :title :initform "Permission" :accessor confirm-dialog-title)
   (message :initarg :message :initform "" :accessor confirm-dialog-message
            :documentation "String or runs; newlines split lines.")
   (choices :initarg :choices :initform +permission-choices+
            :accessor confirm-dialog-choices)
   (feedback-choice :initarg :feedback-choice :initform :deny
                    :accessor confirm-dialog-feedback-choice
                    :documentation "The choice that asks for feedback; NIL: none does.")
   (feedback-prompt :initarg :feedback-prompt :initform "What should it do instead?"
                    :accessor confirm-dialog-feedback-prompt))
  (:documentation "Choose one of CHOICES; the feedback choice collects a line
of text first."))

(defun confirm-dialog-selected (dialog)
  "Index of the selected choice."
  (declare (ignore dialog))
  (%todo 'confirm-dialog-selected))

(defun confirm-dialog-mode (dialog)
  ":CHOOSE or :FEEDBACK."
  (declare (ignore dialog))
  (%todo 'confirm-dialog-mode))

(defun confirm-dialog-feedback-input (dialog)
  "The TEXT-INPUT that collects feedback."
  (declare (ignore dialog))
  (%todo 'confirm-dialog-feedback-input))

(defmethod widget-update ((dialog confirm-dialog) message)
  (declare (ignore message))
  (%todo 'widget-update))

(defmethod widget-view ((dialog confirm-dialog) canvas)
  (declare (ignore canvas))
  (%todo 'widget-view))
