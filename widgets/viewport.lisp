(in-package #:tui-protocol/widgets)

;;; ------------------------------------------------------------------
;;; viewport: a scrolling transcript.
;;;
;;; LINES are logical lines (lists of runs). With WRAP (default) each is
;;; WRAP-RUNS'd at WIDGET-WIDTH into visual rows; without, each is one row
;;; clipped at the right edge. OFFSET is the first visible visual row,
;;; always in [0, MAX-OFFSET] where MAX-OFFSET = max(0, rows - height).
;;;
;;; Scrolling (focused): Up / Down 1 row, Page Up / Page Down WIDGET-HEIGHT
;;; rows, Home / End to the top / bottom, mouse :WHEEL-UP / :WHEEL-DOWN
;;; 3 rows (mouse messages are not filtered by focus). Each scroll sets
;;; FOLLOW-P to AT-BOTTOM-P after it.
;;;
;;; Follow-tail: while FOLLOW-P, appending, replacing the lines and drawing
;;; at a new size keep OFFSET at MAX-OFFSET. Without it, OFFSET only changes
;;; by scrolling and is clamped when the content or size changes.
;;;
;;; View: visual rows [OFFSET, OFFSET + height) from row 0, column 0. No
;;; scrollbar.

(defclass viewport (widget)
  ((initial-lines :initarg :lines :initform '())
   (wrap :initarg :wrap :initform t :accessor viewport-wrap-p)
   (follow :initarg :follow :initform nil :accessor viewport-follow-p))
  (:documentation "Scrollable lines with word wrap and follow-tail.
:LINES takes strings (split at newlines) or run lists."))

(defun viewport-lines (viewport)
  "The logical lines, each a list of runs."
  (declare (ignore viewport))
  (%todo 'viewport-lines))

(defun (setf viewport-lines) (lines viewport)
  "Replace the lines (strings are split at newlines)."
  (declare (ignore lines viewport))
  (%todo '(setf viewport-lines)))

(defun viewport-offset (viewport)
  (declare (ignore viewport))
  (%todo 'viewport-offset))

(defun (setf viewport-offset) (offset viewport)
  "Set the first visible row, clamped to [0, MAX-OFFSET]. FOLLOW-P is not
changed."
  (declare (ignore offset viewport))
  (%todo '(setf viewport-offset)))

(defun viewport-append (viewport &rest lines)
  "Add LINES at the end. A string is split at newlines into several lines; a
list of runs is one line. → VIEWPORT."
  (declare (ignore viewport lines))
  (%todo 'viewport-append))

(defun viewport-append-text (viewport string &key style)
  "Stream STRING onto the end of the last line (a first line is made when
there is none); each #\\Newline in it starts a new line. STYLE (a TP:STYLE)
styles the new text. → VIEWPORT."
  (declare (ignore viewport string style))
  (%todo 'viewport-append-text))

(defun viewport-row-count (viewport)
  "Visual rows at the current width."
  (declare (ignore viewport))
  (%todo 'viewport-row-count))

(defun viewport-max-offset (viewport)
  (declare (ignore viewport))
  (%todo 'viewport-max-offset))

(defun viewport-at-bottom-p (viewport)
  (declare (ignore viewport))
  (%todo 'viewport-at-bottom-p))

(defmethod widget-update ((viewport viewport) message)
  (declare (ignore message))
  (%todo 'widget-update))

(defmethod widget-view ((viewport viewport) canvas)
  (declare (ignore canvas))
  (%todo 'widget-view))
