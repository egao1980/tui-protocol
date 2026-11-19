(in-package #:tui-protocol/widgets/tests)

;;; ------------------------------------------------------------------
;;; spinner

(defun tick-message () (tui:make-tick-message :time 1))

(deftest spinner-defaults-and-init
  (let ((s (make-instance 'w:spinner)))
    (ok (equal w:+braille-frames+ (w:spinner-frames s)))
    (ok (= 10 (length w:+braille-frames+)))
    (ok (= 0 (w:spinner-frame s)))
    (ok (= 0.1 (w:spinner-interval s)))
    (ok (w:spinner-active-p s))
    (ok (equal (tui:tick 0.1) (w:widget-init s)) "the first tick is scheduled"))
  (ok (null (w:widget-init (make-instance 'w:spinner :active nil))) "inactive: no tick"))

(deftest spinner-ticks
  (let ((s (make-instance 'w:spinner :frames '("a" "b" "c") :interval 0.5)))
    (multiple-value-bind (s events commands) (feed s (tick-message))
      (ok (= 1 (w:spinner-frame s)))
      (ok (null events))
      (ok (equal (list (tui:tick 0.5)) commands) "each tick schedules the next"))
    (feed s (tick-message) (tick-message))
    (ok (= 0 (w:spinner-frame s)) "wraps")
    (feed s (key :a) (paste "x"))
    (ok (= 0 (w:spinner-frame s)) "keys and paste are ignored")))

(deftest spinner-start-stop
  (let ((s (make-instance 'w:spinner :frames '("a" "b") :interval 0.2)))
    (ok (null (w:spinner-stop s)))
    (ok (not (w:spinner-active-p s)))
    (multiple-value-bind (s events commands) (feed s (tick-message))
      (ok (= 0 (w:spinner-frame s)) "inactive: the tick is ignored")
      (ok (null events))
      (ok (null commands) "and the chain ends"))
    (ok (equal (tui:tick 0.2) (w:spinner-start s)))
    (ok (w:spinner-active-p s))))

(deftest spinner-view
  (let ((s (make-instance 'w:spinner :label "Thinking")))
    (ok (equal '("⠋ Thinking" "") (lines s 20 2)))
    (feed s (tick-message))
    (ok (equal '("⠙ Thinking" "") (lines s 20 2)))
    (setf (w:spinner-label s) "")
    (ok (equal '("⠙" "") (lines s 20 2)) "no label: just the frame")
    (w:spinner-stop s)
    (ok (equal '("" "") (lines s 20 2)) "inactive: nothing")))

(deftest spinner-e2e
  (let ((s (make-instance 'w:spinner :label "Working" :interval 3600)))
    (with-widget-tui (tui program s :cols 20 :rows 1)
      (ok (screen-is tui '("⠋ Working")) "the first frame")))
  (let ((s (make-instance 'w:spinner :label "Working" :frames '("a" "b") :interval 0.05)))
    (with-widget-tui (tui program s :cols 20 :rows 1)
      (ok (screen-is tui '("b Working") :timeout 3) "ticks from the loop advance it"))))

;;; ------------------------------------------------------------------
;;; status-line

(defun status (&rest initargs) (apply #'make-instance 'w:status-line initargs))

(deftest status-line-layout
  (ok (equal '("main | opus      12k tok" "")
             (lines (status :left "main | opus" :right "12k tok") 24 2))
      "left at 0, right ends at the last column; row 1 untouched")
  (ok (equal '("main |… 12k tok") (lines (status :left "main | opus" :right "12k tok") 15 1))
      "left cut to W - RW - 1")
  (ok (equal '("main | …") (lines (status :left "main | opus" :right "12k tok") 8 1))
      "RW + 2 > W: right dropped, left cut to W")
  (ok (equal '("abcd…") (lines (status :left "abcdefg") 5 1)) "no right")
  (ok (equal '("     right") (lines (status :right "right") 10 1)) "no left"))

(deftest status-line-styles
  (let* ((inv (tp:make-style :inverse t))
         (canvas (draw (status :left "ab" :right (w:text-run "ok" :bold t) :style inv) 10 2)))
    (ok (tp:style-inverse (cell-style canvas 0 0)) "a string is drawn in STYLE")
    (ok (tp:style-inverse (cell-style canvas 5 0)) "the row is filled with STYLE")
    (ok (tp:style-bold (cell-style canvas 8 0)) "runs keep their style")
    (ok (tp:style-bold (cell-style canvas 9 0)))
    (ok (not (tp:style-inverse (cell-style canvas 0 1))) "only row 0")))

(deftest status-line-ignores-messages
  (let ((s (status :left "x")))
    (multiple-value-bind (s events commands) (feed s "abc" (key :enter))
      (ok (string= "x" (w:status-line-left s)))
      (ok (null events))
      (ok (null commands)))))

(deftest status-line-e2e
  (let ((s (status :left "main | opus" :right "12k tok")))
    (with-widget-tui (tui program s :cols 24 :rows 2)
      (ok (screen-is tui '("main | opus      12k tok"))))))
