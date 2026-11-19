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
  (setf (spinner-active-p spinner) t)
  (tui:tick (spinner-interval spinner)))

(defun spinner-stop (spinner)
  "Deactivate SPINNER (its pending tick is then ignored). → NIL."
  (setf (spinner-active-p spinner) nil)
  nil)

(defmethod widget-init ((spinner spinner))
  (when (spinner-active-p spinner)
    (tui:tick (spinner-interval spinner))))

(defmethod widget-update ((spinner spinner) message)
  (if (and (tui:tick-message-p message) (spinner-active-p spinner))
      (let ((n (length (spinner-frames spinner))))
        (setf (spinner-frame spinner)
              (if (plusp n) (mod (1+ (spinner-frame spinner)) n) 0))
        (values spinner (tui:tick (spinner-interval spinner)) nil))
      (values spinner nil nil)))

(defmethod widget-view ((spinner spinner) canvas)
  (setf (widget-width spinner) (tui:canvas-cols canvas)
        (widget-height spinner) (tui:canvas-rows canvas))
  (let ((frames (spinner-frames spinner)))
    (when (and (spinner-active-p spinner) frames)
      (let ((x (draw-runs canvas 0 0 (nth (mod (spinner-frame spinner) (length frames)) frames)))
            (label (spinner-label spinner)))
        (when (plusp (runs-width label))
          (draw-runs canvas (1+ x) 0 label))))))

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

(defun %side-runs (side style)
  "A status-line side: a string is drawn in STYLE, runs in their own styles."
  (if (stringp side)
      (if (zerop (length side)) '() (list (text-run side :style style)))
      (as-runs side)))

(defmethod widget-view ((status status-line) canvas)
  (let* ((w (tui:canvas-cols canvas))
         (style (status-line-style status))
         (left (%side-runs (status-line-left status) style))
         (right (%side-runs (status-line-right status) style))
         (rw (runs-width right)))
    (setf (widget-width status) w (widget-height status) (tui:canvas-rows canvas))
    (tui:fill-rect canvas 0 0 w 1 :style style)
    (if (or (zerop rw) (> (+ rw 2) w))
        (draw-runs canvas 0 0 (truncate-runs left w))
        (progn (draw-runs canvas 0 0 (truncate-runs left (- w rw 1)))
               (draw-runs canvas (- w rw) 0 right)))))
