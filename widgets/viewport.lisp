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
   (follow :initarg :follow :initform nil :accessor viewport-follow-p)
   (lines :initform (make-array 0 :adjustable t :fill-pointer 0)
          :documentation "Logical lines, each a list of runs.")
   (offset :initform 0 :type (integer 0))
   (rows :initform nil
         :documentation "Cache (width wrap . visual-row-vector); NIL after a change."))
  (:documentation "Scrollable lines with word wrap and follow-tail.
:LINES takes strings (split at newlines) or run lists."))

(defun %line-items (item)
  "One :LINES / append argument → a list of lines (lists of runs)."
  (if (stringp item)
      (mapcar #'as-runs (%split-lines item))
      (list (as-runs item))))

(defun %add-lines (viewport items)
  (let ((lines (slot-value viewport 'lines)))
    (dolist (item items)
      (dolist (line (%line-items item))
        (vector-push-extend line lines)))
    (setf (slot-value viewport 'rows) nil)))

(defun %rows (viewport)
  "Visual rows at WIDGET-WIDTH, cached until the content, width or wrap changes."
  (let ((width (max 1 (widget-width viewport)))
        (wrap (viewport-wrap-p viewport))
        (cache (slot-value viewport 'rows)))
    (if (and cache (eql (first cache) width) (eq (second cache) wrap))
        (cddr cache)
        (let ((rows (coerce (loop for line across (slot-value viewport 'lines)
                                  if wrap append (wrap-runs line width)
                                    else collect line)
                            'vector)))
          (setf (slot-value viewport 'rows) (list* width wrap rows))
          rows))))

(defun viewport-row-count (viewport)
  "Visual rows at the current width."
  (length (%rows viewport)))

(defun viewport-max-offset (viewport)
  (max 0 (- (viewport-row-count viewport) (widget-height viewport))))

(defun viewport-offset (viewport)
  (slot-value viewport 'offset))

(defun viewport-at-bottom-p (viewport)
  (>= (viewport-offset viewport) (viewport-max-offset viewport)))

(defun %settle (viewport)
  "After a content or size change: at the tail while following, else clamped."
  (setf (slot-value viewport 'offset)
        (if (viewport-follow-p viewport)
            (viewport-max-offset viewport)
            (max 0 (min (slot-value viewport 'offset) (viewport-max-offset viewport))))))

(defmethod initialize-instance :after ((viewport viewport) &key)
  (let ((initial (slot-value viewport 'initial-lines)))
    (%add-lines viewport (if (stringp initial) (list initial) initial)))
  (%settle viewport))

(defmethod (setf viewport-follow-p) :after (value (viewport viewport))
  (declare (ignore value))
  (%settle viewport))

(defmethod (setf viewport-wrap-p) :after (value (viewport viewport))
  (declare (ignore value))
  (%settle viewport))

(defun viewport-lines (viewport)
  "The logical lines, each a list of runs."
  (coerce (slot-value viewport 'lines) 'list))

(defun (setf viewport-lines) (lines viewport)
  "Replace the lines (strings are split at newlines)."
  (setf (fill-pointer (slot-value viewport 'lines)) 0)
  (%add-lines viewport (if (stringp lines) (list lines) lines))
  (%settle viewport)
  lines)

(defun (setf viewport-offset) (offset viewport)
  "Set the first visible row, clamped to [0, MAX-OFFSET]. FOLLOW-P is not
changed."
  (check-type offset integer)
  (setf (slot-value viewport 'offset)
        (max 0 (min offset (viewport-max-offset viewport)))))

(defun viewport-append (viewport &rest lines)
  "Add LINES at the end. A string is split at newlines into several lines; a
list of runs is one line. → VIEWPORT."
  (%add-lines viewport lines)
  (%settle viewport)
  viewport)

(defun viewport-append-text (viewport string &key style)
  "Stream STRING onto the end of the last line (a first line is made when
there is none); each #\\Newline in it starts a new line. STYLE (a TP:STYLE)
styles the new text. → VIEWPORT."
  (check-type string string)
  (check-type style (or null tp:style))
  (let ((lines (slot-value viewport 'lines))
        (style (or style tp:+default-style+)))
    (when (zerop (length lines))
      (vector-push-extend '() lines))
    (loop for piece in (%split-lines string)
          for first = t then nil
          do (unless first
               (vector-push-extend '() lines))
             (when (plusp (length piece))
               (let* ((i (1- (length lines)))
                      (line (aref lines i))
                      (last-run (car (last line))))
                 (setf (aref lines i)
                       (if (and last-run (eq style (text-run-style last-run)))
                           ;; Same style: extend the last run (a fresh one, never mutated).
                           (append (butlast line)
                                   (list (%make-text-run (concatenate 'string
                                                                      (text-run-text last-run)
                                                                      piece)
                                                         style)))
                           (append line (list (%make-text-run piece style))))))))
    (setf (slot-value viewport 'rows) nil))
  (%settle viewport)
  viewport)

(defun %scroll-to (viewport offset)
  (setf (viewport-offset viewport) offset)
  (setf (slot-value viewport 'follow) (viewport-at-bottom-p viewport)))

(defmethod widget-update ((viewport viewport) message)
  (let ((offset (viewport-offset viewport))
        (height (widget-height viewport)))
    (typecase message
      (tui:key-message
       (case (tui:key-message-key message)
         (:arrow-up (%scroll-to viewport (1- offset)))
         (:arrow-down (%scroll-to viewport (1+ offset)))
         (:page-up (%scroll-to viewport (- offset height)))
         (:page-down (%scroll-to viewport (+ offset height)))
         (:home (%scroll-to viewport 0))
         (:end (%scroll-to viewport (viewport-max-offset viewport)))))
      (tui:mouse-message
       (when (eq :press (tui:mouse-message-action message))
         (case (tui:mouse-message-button message)
           (:wheel-up (%scroll-to viewport (- offset 3)))
           (:wheel-down (%scroll-to viewport (+ offset 3)))))))
    (values viewport nil nil)))

(defmethod widget-view ((viewport viewport) canvas)
  (setf (widget-width viewport) (tui:canvas-cols canvas)
        (widget-height viewport) (tui:canvas-rows canvas))
  (%settle viewport)
  (let ((rows (%rows viewport))
        (offset (viewport-offset viewport)))
    (loop for i from offset below (min (length rows) (+ offset (tui:canvas-rows canvas)))
          for y from 0
          do (draw-runs canvas 0 y (aref rows i)))))
