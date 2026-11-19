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
;;; top = clamp(clamp(last-top, selected - H + 1, selected), 0, max(0, items - H)):
;;; the window remembers the last top drawn and scrolls only as far as the
;;; selection needs (the tests pin this). An empty list draws nothing.

(defclass select-list (widget)
  ((items :initarg :items :initform '())
   (label-function :initarg :label-function :initform #'princ-to-string
                   :accessor select-list-label-function
                   :documentation "Item → string or runs.")
   (initial-selected :initarg :selected :initform 0)
   (selected :initform 0 :type (integer 0))
   (top :initform 0 :type (integer 0)
        :documentation "First row drawn last time; the window scrolls only as far as
the selection needs."))
  (:documentation "A vertical list with one selected item."))

(defun %clamp-index (index count)
  (max 0 (min index (1- count))))

(defmethod initialize-instance :after ((list select-list) &key)
  (setf (select-list-selected list) (slot-value list 'initial-selected)))

(defun select-list-items (list)
  (slot-value list 'items))

(defun (setf select-list-items) (items list)
  "Replace the items; the selection is clamped to the new list (0 when empty)."
  (check-type items list)
  (setf (slot-value list 'items) items)
  (setf (select-list-selected list) (slot-value list 'selected))
  items)

(defun select-list-selected (list)
  "Index of the selected item (0 for an empty list)."
  (slot-value list 'selected))

(defun (setf select-list-selected) (index list)
  "Select INDEX, clamped to the items."
  (check-type index integer)
  (setf (slot-value list 'selected)
        (%clamp-index index (length (slot-value list 'items)))))

(defun select-list-selected-item (list)
  "The selected item, NIL for an empty list."
  (nth (slot-value list 'selected) (slot-value list 'items)))

(defmethod widget-update ((list select-list) message)
  (let ((event nil))
    (when (tui:key-message-p message)
      (let ((selected (select-list-selected list))
            (height (widget-height list))
            (count (length (select-list-items list))))
        (case (tui:key-message-key message)
          (:arrow-up (setf (select-list-selected list) (1- selected)))
          (:arrow-down (setf (select-list-selected list) (1+ selected)))
          (:home (setf (select-list-selected list) 0))
          (:end (setf (select-list-selected list) (1- count)))
          (:page-up (setf (select-list-selected list) (- selected height)))
          (:page-down (setf (select-list-selected list) (+ selected height)))
          (:enter (when (plusp count)
                    (setf event (make-select-event :widget list :index selected
                                                   :item (select-list-selected-item list)))))
          (:escape (setf event (make-cancel-event :widget list))))))
    (values list nil event)))

(defun %bold-runs (runs)
  "RUNS with bold added (fresh runs; the originals are not mutated)."
  (mapcar (lambda (run)
            (%make-text-run (text-run-text run)
                            (let ((s (tp:copy-style (text-run-style run))))
                              (setf (tp:style-bold s) t)
                              s)))
          (as-runs runs)))

(defmethod widget-view ((list select-list) canvas)
  (let* ((w (tui:canvas-cols canvas))
         (h (tui:canvas-rows canvas))
         (items (select-list-items list))
         (n (length items))
         (selected (select-list-selected list)))
    (setf (widget-width list) w (widget-height list) h)
    (let ((top (max 0 (min (max (- selected h -1) (min (slot-value list 'top) selected))
                           (max 0 (- n h))))))
      (setf (slot-value list 'top) top)
      (loop for i from top below (min n (+ top h))
            for item in (nthcdr top items)
            for y from 0
            do (let ((runs (cons (text-run (if (= i selected) "> " "  "))
                                 (truncate-runs (as-runs (funcall (select-list-label-function list)
                                                                  item))
                                                (max 0 (- w 2))))))
                 (draw-runs canvas 0 y (if (= i selected) (%bold-runs runs) runs)))))))

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
                    :accessor confirm-dialog-feedback-prompt)
   (selected :initform 0 :type (integer 0))
   (mode :initform :choose :type (member :choose :feedback))
   (feedback-input :initform nil))
  (:documentation "Choose one of CHOICES; the feedback choice collects a line
of text first."))

(defmethod initialize-instance :after ((dialog confirm-dialog) &key)
  (setf (slot-value dialog 'feedback-input)
        (make-instance 'text-input :prompt "> " :focused (widget-focused-p dialog))))

(defmethod (setf widget-focused-p) :after (value (dialog confirm-dialog))
  ;; The feedback input follows the dialog's focus.
  (let ((input (slot-value dialog 'feedback-input)))
    (when input
      (setf (widget-focused-p input) value))))

(defun confirm-dialog-selected (dialog)
  "Index of the selected choice."
  (slot-value dialog 'selected))

(defun confirm-dialog-mode (dialog)
  ":CHOOSE or :FEEDBACK."
  (slot-value dialog 'mode))

(defun confirm-dialog-feedback-input (dialog)
  "The TEXT-INPUT that collects feedback."
  (slot-value dialog 'feedback-input))

(defun %choose (dialog index)
  "Choose choice INDEX. → an event, or NIL when it switches to :FEEDBACK."
  (let ((choice (first (nth index (confirm-dialog-choices dialog)))))
    (setf (slot-value dialog 'selected) index)
    (if (and choice (eq choice (confirm-dialog-feedback-choice dialog)))
        (progn (setf (slot-value dialog 'mode) :feedback) nil)
        (make-confirm-event :widget dialog :choice choice :feedback nil))))

(defun %digit (message)
  "The digit 1-9 a key message types without Ctrl/Alt/Super, or NIL."
  (let ((text (tui:key-message-text message)))
    (and text (= 1 (length text))
         (null (intersection (tui:key-message-modifiers message) '(:ctrl :alt :super)))
         (let ((d (digit-char-p (char text 0))))
           (and d (<= 1 d 9) d)))))

(defun %choose-key (dialog message)
  (let* ((n (length (confirm-dialog-choices dialog)))
         (selected (confirm-dialog-selected dialog))
         (digit (%digit message)))
    (cond (digit (when (<= digit n) (%choose dialog (1- digit))))
          (t (case (tui:key-message-key message)
               (:arrow-up (setf (slot-value dialog 'selected) (%clamp-index (1- selected) n)) nil)
               (:arrow-down (setf (slot-value dialog 'selected) (%clamp-index (1+ selected) n)) nil)
               (:enter (when (plusp n) (%choose dialog selected)))
               (:escape (make-cancel-event :widget dialog))
               (t nil))))))

(defmethod widget-update ((dialog confirm-dialog) message)
  (let ((input (confirm-dialog-feedback-input dialog)))
    (setf (widget-focused-p input) (widget-focused-p dialog))
    (ecase (confirm-dialog-mode dialog)
      (:choose
       (values dialog nil (and (tui:key-message-p message) (%choose-key dialog message))))
      (:feedback
       (cond ((and (tui:key-message-p message)
                   (eq :enter (tui:key-message-key message))
                   (null (tui:key-message-modifiers message)))
              (values dialog nil
                      (make-confirm-event :widget dialog
                                          :choice (confirm-dialog-feedback-choice dialog)
                                          :feedback (text-input-value input))))
             ((and (tui:key-message-p message)
                   (eq :escape (tui:key-message-key message)))
              (setf (slot-value dialog 'mode) :choose)
              (values dialog nil nil))
             (t
              (multiple-value-bind (widget command event) (widget-update input message)
                (declare (ignore widget))
                ;; Only Ctrl+G's edit-event (naming the input) is passed on.
                (values dialog command (and (edit-event-p event) event)))))))))

(defmethod widget-view ((dialog confirm-dialog) canvas)
  (let* ((w (tui:canvas-cols canvas))
         (h (tui:canvas-rows canvas))
         (iw (max 1 (- w 4)))
         (inner (tui:sub-canvas canvas 2 1 (- w 4) (- h 2)))
         (input (confirm-dialog-feedback-input dialog))
         (y 0))
    (setf (widget-width dialog) w (widget-height dialog) h)
    (setf (widget-focused-p input) (widget-focused-p dialog))
    (tui:draw-box canvas 0 0 w h :title (confirm-dialog-title dialog))
    (dolist (line (wrap-runs (confirm-dialog-message dialog) iw))
      (draw-runs inner 0 y line)
      (incf y))
    (incf y)
    (loop for (nil label) in (confirm-dialog-choices dialog)
          for i from 0
          for selected = (= i (confirm-dialog-selected dialog))
          do (let ((runs (cons (text-run (format nil "~:[  ~;> ~]~D. " selected (1+ i)))
                               (as-runs label))))
               (draw-runs inner 0 y (if selected (%bold-runs runs) runs))
               (incf y)))
    (when (eq :feedback (confirm-dialog-mode dialog))
      (draw-runs inner 0 y (truncate-runs (confirm-dialog-feedback-prompt dialog) iw))
      (incf y)
      (widget-view input (tui:sub-canvas inner 0 y iw 1)))))
