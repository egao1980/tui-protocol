(in-package #:tui-protocol/widgets)

;;; ------------------------------------------------------------------
;;; spinner
;;;
;;; WIDGET-INIT → (TUI:TICK interval) when active, else NIL. A
;;; TUI:TICK-MESSAGE advances an active spinner one frame (wrapping) and
;;; returns (TUI:TICK interval) again; an inactive one ignores it, which ends
;;; the tick chain. Keys are ignored.
;;; View: active → frame, then " " + label when the label is not empty, at
;;; row 0, column 0; inactive → nothing.

(defparameter +braille-frames+ '("⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏"))

(defclass spinner (widget)
  ((frames :initarg :frames :initform +braille-frames+ :accessor spinner-frames)
   (frame :initform 0 :accessor spinner-frame
          :documentation "Index into FRAMES.")
   (interval :initarg :interval :initform 0.1 :accessor spinner-interval)
   (label :initarg :label :initform "" :accessor spinner-label
          :documentation "String or runs drawn after the frame.")
   (active :initarg :active :initform t :accessor spinner-active-p))
  (:documentation "A frame animation driven by tick messages."))

(defun spinner-start (spinner)
  "Activate SPINNER. → the command that starts its ticks."
  (declare (ignore spinner))
  (%todo 'spinner-start))

(defun spinner-stop (spinner)
  "Deactivate SPINNER (its pending tick is then ignored). → NIL."
  (declare (ignore spinner))
  (%todo 'spinner-stop))

(defmethod widget-init ((spinner spinner))
  (%todo 'widget-init))

(defmethod widget-update ((spinner spinner) message)
  (declare (ignore message))
  (%todo 'widget-update))

(defmethod widget-view ((spinner spinner) canvas)
  (declare (ignore canvas))
  (%todo 'widget-view))

;;; ------------------------------------------------------------------
;;; status-line
;;;
;;; One row (row 0). The whole row is filled with STYLE (TUI:FILL-RECT), then
;;; LEFT from column 0 and RIGHT right-aligned to end at the last column. A
;;; string is drawn in STYLE, runs in their own styles. With LW, RW the
;;; widths and W the canvas width: when RW = 0, LEFT is truncated to W; when RW + 2 > W, RIGHT is
;;; dropped and LEFT truncated to W; otherwise LEFT is truncated to
;;; W - RW - 1 (TRUNCATE-RUNS, so a cut left ends in "…"). Messages are
;;; ignored.

(defclass status-line (widget)
  ((left :initarg :left :initform "" :accessor status-line-left)
   (right :initarg :right :initform "" :accessor status-line-right)
   (style :initarg :style :initform tp:+default-style+ :accessor status-line-style))
  (:documentation "A one-row bar: left text, right-aligned text."))

(defmethod widget-view ((status status-line) canvas)
  (declare (ignore canvas))
  (%todo 'widget-view))
