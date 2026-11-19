(in-package #:tui-protocol/widgets/tests)

;;; ------------------------------------------------------------------
;;; select-list

(defun select-list (&rest initargs) (apply #'make-instance 'w:select-list initargs))

(defparameter *greek* '("alpha" "beta" "gamma" "delta"))

(deftest select-list-moves
  (let ((l (select-list :items *greek* :height 2)))
    (ok (= 0 (w:select-list-selected l)))
    (ok (string= "alpha" (w:select-list-selected-item l)))
    (feed l (key :arrow-down))
    (ok (= 1 (w:select-list-selected l)))
    (feed l (repeat 5 (key :arrow-down)))
    (ok (= 3 (w:select-list-selected l)) "clamped, no wrap")
    (feed l (key :home))
    (ok (= 0 (w:select-list-selected l)))
    (feed l (key :arrow-up))
    (ok (= 0 (w:select-list-selected l)))
    (feed l (key :end))
    (ok (= 3 (w:select-list-selected l)))
    (feed l (key :page-up))
    (ok (= 1 (w:select-list-selected l)) "a page is the height")
    (feed l (key :page-down) (key :page-down))
    (ok (= 3 (w:select-list-selected l)))))

(deftest select-list-events
  (let ((l (select-list :items *greek*)))
    (multiple-value-bind (l events) (feed l (key :end) (key :enter))
      (declare (ignore l))
      (ok (= 1 (length events)))
      (ok (w:select-event-p (first events)))
      (ok (= 3 (w:select-event-index (first events))))
      (ok (string= "delta" (w:select-event-item (first events)))))
    (multiple-value-bind (l events) (feed l (key :escape))
      (declare (ignore l))
      (ok (w:cancel-event-p (first events)))))
  (let ((l (select-list)))
    (multiple-value-bind (l events) (feed l (key :enter) (key :arrow-down))
      (ok (null events) "an empty list chooses nothing")
      (ok (= 0 (w:select-list-selected l)))
      (ok (null (w:select-list-selected-item l)))
      (ok (equal '("" "") (lines l 10 2))))))

(deftest select-list-items-and-selection
  (let ((l (select-list :items *greek* :selected 2)))
    (ok (= 2 (w:select-list-selected l)))
    (setf (w:select-list-selected l) 10)
    (ok (= 3 (w:select-list-selected l)) "setf clamps")
    (setf (w:select-list-items l) '("x" "y"))
    (ok (= 1 (w:select-list-selected l)) "fewer items: clamped")
    (ok (equal '("x" "y") (w:select-list-items l)))))

(deftest select-list-view
  (let ((l (select-list :items *greek*)))
    (let ((canvas (draw l 12 3)))
      (ok (equal '("> alpha" "  beta" "  gamma") (tui:canvas-lines canvas)))
      (ok (tp:style-bold (cell-style canvas 2 0)) "the selected row is bold")
      (ok (not (tp:style-bold (cell-style canvas 2 1)))))
    (feed l (key :end))
    (ok (equal '("  beta" "  gamma" "> delta") (lines l 12 3)) "scrolled to the selection")
    (feed l (key :arrow-up))
    (ok (equal '("  beta" "> gamma" "  delta") (lines l 12 3)) "stateless window")
    (ok (equal '("  alpha" "  beta" "> gamma" "  delta" "") (lines l 12 5))))
  (let ((l (select-list :items '(1 2) :label-function (lambda (n) (format nil "item ~D" n)))))
    (ok (equal '("> item 1" "  item 2") (lines l 12 2))))
  (let ((l (select-list :items '("a-very-long-label"))))
    (ok (equal '("> a-ver…") (lines l 8 1)) "labels cut to W - 2")))

(deftest select-list-e2e
  (let ((l (select-list :items *greek*)))
    (with-widget-tui (tui program l :cols 12 :rows 3 :quit-on '(w:select-event))
      (ok (screen-is tui '("> alpha" "  beta" "  gamma")))
      (h:tui-press tui :arrow-down)
      (h:tui-press tui :arrow-down)
      (ok (screen-is tui '("  alpha" "  beta" "> gamma")))
      (h:tui-press tui :enter)
      (ok (eq l (h:tui-result tui :timeout 3)))
      (let ((event (first (w:widget-program-events program))))
        (ok (and event (= 2 (w:select-event-index event))))
        (ok (and event (string= "gamma" (w:select-event-item event))))))))

;;; ------------------------------------------------------------------
;;; confirm-dialog

(defparameter *bash-message* "Run rm -rf build in /tmp/project now?")

(defun dialog (&rest initargs)
  (apply #'make-instance 'w:confirm-dialog :title "Allow bash?" :message *bash-message*
         initargs))

(defun boxed (width height title rows)
  "The expected lines of a W × H TUI:DRAW-BOX with TITLE and, inside, ROWS
(an alist (y . text)) drawn from column 2."
  (loop for y below height
        collect (cond ((= y 0)
                       (format nil "┌─~A~A┐" title
                               (make-string (- width (length title) 3) :initial-element #\─)))
                      ((= y (1- height))
                       (format nil "└~A┘" (make-string (- width 2) :initial-element #\─)))
                      (t (let ((text (or (cdr (assoc y rows)) "")))
                           (format nil "│ ~A~A│" text
                                   (make-string (- width 3 (length text))
                                                :initial-element #\Space)))))))

(deftest boxed-helper-matches-draw-box
  ;; The helper is checked against the real primitives, so the dialog
  ;; snapshots below cannot drift from what DRAW-BOX / DRAW-TEXT produce.
  (let ((canvas (tui:make-canvas 30 5)))
    (tui:draw-box canvas 0 0 30 5 :title "Allow bash?")
    (tui:draw-text canvas 2 1 "hello")
    (tui:draw-text canvas 2 3 "> 1. Allow once")
    (ok (equal (boxed 30 5 "Allow bash?" '((1 . "hello") (3 . "> 1. Allow once")))
               (tui:canvas-lines canvas)))))

(deftest confirm-dialog-defaults
  (let ((d (dialog)))
    (ok (equal '((:allow-once "Allow once") (:allow-always "Always allow") (:deny "Deny"))
               (w:confirm-dialog-choices d)))
    (ok (eq :deny (w:confirm-dialog-feedback-choice d)))
    (ok (= 0 (w:confirm-dialog-selected d)))
    (ok (eq :choose (w:confirm-dialog-mode d)))
    (ok (typep (w:confirm-dialog-feedback-input d) 'w:text-input))))

(deftest confirm-dialog-choose
  (let ((d (dialog)))
    (feed d (key :arrow-down))
    (ok (= 1 (w:confirm-dialog-selected d)))
    (feed d (key :arrow-down) (key :arrow-down))
    (ok (= 2 (w:confirm-dialog-selected d)) "clamped")
    (feed d (key :arrow-up))
    (multiple-value-bind (d events) (feed d (key :enter))
      (declare (ignore d))
      (ok (= 1 (length events)))
      (ok (w:confirm-event-p (first events)))
      (ok (eq :allow-always (w:confirm-event-choice (first events))))
      (ok (null (w:confirm-event-feedback (first events))))))
  (let ((d (dialog)))
    (multiple-value-bind (d events) (feed d "9")
      (ok (null events) "no choice 9")
      (ok (= 0 (w:confirm-dialog-selected d))))
    (multiple-value-bind (d events) (feed d "1")
      (declare (ignore d))
      (ok (eq :allow-once (w:confirm-event-choice (first events))) "digits choose")))
  (let ((d (dialog)))
    (multiple-value-bind (d events) (feed d (key :escape))
      (declare (ignore d))
      (ok (w:cancel-event-p (first events))))))

(deftest confirm-dialog-feedback
  (let ((d (dialog)))
    (multiple-value-bind (d events) (feed d (key :arrow-down) (key :arrow-down) (key :enter))
      (ok (null events) "the feedback choice asks first")
      (ok (eq :feedback (w:confirm-dialog-mode d))))
    (multiple-value-bind (d events) (feed d "use trash" (key :enter))
      (declare (ignore d))
      (ok (= 1 (length events)))
      (ok (eq :deny (w:confirm-event-choice (first events))))
      (ok (string= "use trash" (w:confirm-event-feedback (first events))))))
  (let ((d (dialog)))
    (multiple-value-bind (d events) (feed d "3" (key :enter))
      (declare (ignore d))
      (ok (= 1 (length events)))
      (ok (string= "" (w:confirm-event-feedback (first events))) "empty feedback is allowed")))
  (let ((d (dialog)))
    (feed d "3" "abc")
    (multiple-value-bind (d events) (feed d (key :escape))
      (ok (null events))
      (ok (eq :choose (w:confirm-dialog-mode d)) "Escape goes back to the choices")
      (ok (string= "abc" (w:text-input-value (w:confirm-dialog-feedback-input d))) "text kept"))
    (multiple-value-bind (d events) (feed d (key :escape))
      (declare (ignore d))
      (ok (w:cancel-event-p (first events)))))
  (let ((d (dialog :feedback-choice nil)))
    (multiple-value-bind (d events) (feed d "3")
      (ok (eq :deny (w:confirm-event-choice (first events))))
      (ok (null (w:confirm-event-feedback (first events))))
      (ok (eq :choose (w:confirm-dialog-mode d)))))
  (let ((d (dialog :choices '((:yes "Yes") (:no "No")) :feedback-choice :no)))
    (multiple-value-bind (d events) (feed d "1")
      (declare (ignore d))
      (ok (eq :yes (w:confirm-event-choice (first events))) "custom choices"))))

(deftest confirm-dialog-view
  (let ((d (dialog)))
    (let ((canvas (draw d 30 9)))
      (ok (equal (boxed 30 9 "Allow bash?"
                        '((1 . "Run rm -rf build in") (2 . "/tmp/project now?")
                          (4 . "> 1. Allow once") (5 . "  2. Always allow") (6 . "  3. Deny")))
                 (tui:canvas-lines canvas)))
      (ok (tp:style-bold (cell-style canvas 4 4)) "the selected choice is bold")
      (ok (not (tp:style-bold (cell-style canvas 4 5)))))
    (feed d "3" "no")
    (ok (equal (boxed 30 10 "Allow bash?"
                      '((1 . "Run rm -rf build in") (2 . "/tmp/project now?")
                        (4 . "  1. Allow once") (5 . "  2. Always allow") (6 . "> 3. Deny")
                        (7 . "What should it do instead?") (8 . "> no")))
               (lines d 30 10)))
    (ok (equal (boxed 30 9 "Allow bash?"
                      '((1 . "Run rm -rf build in") (2 . "/tmp/project now?")
                        (4 . "  1. Allow once") (5 . "  2. Always allow") (6 . "> 3. Deny")
                        (7 . "What should it do instead?")))
               (lines d 30 9))
        "rows past H - 2 are not drawn")))

(deftest confirm-dialog-e2e
  (let ((d (dialog)))
    (with-widget-tui (tui program d :cols 30 :rows 10 :quit-on '(w:confirm-event))
      (ok (screen-is tui (boxed 30 10 "Allow bash?"
                                '((1 . "Run rm -rf build in") (2 . "/tmp/project now?")
                                  (4 . "> 1. Allow once") (5 . "  2. Always allow")
                                  (6 . "  3. Deny")))))
      (h:tui-type tui "3")
      (h:tui-type tui "no")
      (ok (screen-is tui (boxed 30 10 "Allow bash?"
                                '((1 . "Run rm -rf build in") (2 . "/tmp/project now?")
                                  (4 . "  1. Allow once") (5 . "  2. Always allow")
                                  (6 . "> 3. Deny") (7 . "What should it do instead?")
                                  (8 . "> no")))))
      (h:tui-press tui :enter)
      (ok (eq d (h:tui-result tui :timeout 3)))
      (let ((event (first (w:widget-program-events program))))
        (ok (and event (eq :deny (w:confirm-event-choice event))))
        (ok (and event (equal "no" (w:confirm-event-feedback event))))))))
