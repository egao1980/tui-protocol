(in-package #:tui-protocol/widgets/tests)

;;; The widget protocol and WIDGET-PROGRAM.

(deftest view-records-size
  (let ((vp (make-instance 'w:viewport :lines '("a"))))
    (ok (= 80 (w:widget-width vp)))
    (ok (= 24 (w:widget-height vp)))
    (draw vp 12 3)
    (ok (= 12 (w:widget-width vp)))
    (ok (= 3 (w:widget-height vp)))
    (w:widget-view vp (tui:sub-canvas (tui:make-canvas 30 10) 5 5 7 2))
    (ok (equal '(7 2) (list (w:widget-width vp) (w:widget-height vp))) "a sub-canvas's size")))

(deftest update-returns-the-widget
  (let ((input (make-instance 'w:text-input)))
    (multiple-value-bind (widget command event) (w:widget-update input (first (chars "a")))
      (ok (eq input widget) "updated in place and returned")
      (ok (null command))
      (ok (null event)))
    (multiple-value-bind (widget command event)
        (w:widget-update input (tui:make-resize-message :cols 10 :rows 3))
      (ok (eq input widget))
      (ok (null command))
      (ok (null event)))))

(deftest unfocused-ignores-keys-and-paste
  (let ((input (make-instance 'w:text-input :focused nil :value "x")))
    (feed input "abc" (paste "zz") (key :backspace))
    (ok (string= "x" (w:text-input-value input)))
    (ok (null (inverse-cells (draw input 10 2))) "no cursor drawn"))
  (let ((list (make-instance 'w:select-list :items '(a b c) :focused nil)))
    (feed list (key :arrow-down))
    (ok (= 0 (w:select-list-selected list)))))

(deftest release-events-are-ignored
  (let ((input (make-instance 'w:text-input)))
    (feed input (tui:make-key-message :key :a :text "a" :action :release)
          (tui:make-key-message :key :b :text "b" :action :repeat))
    (ok (string= "b" (w:text-input-value input)))))

(deftest widget-program-runs-a-widget
  (let ((input (make-instance 'w:text-input :prompt "> ")))
    (with-widget-tui (tui program input :cols 20 :rows 3)
      (ok (eq input (w:widget-program-widget program)))
      (ok (screen-is tui '(">")))
      (h:tui-type tui "hi")
      (ok (screen-is tui '("> hi")))
      (h:tui-press tui :enter)
      (ok (events-reach program 1))
      (ok (screen-is tui '(">")))
      (let ((events (w:widget-program-events program)))
        (ok (= 1 (length events)))
        (ok (w:submit-event-p (first events)))
        (ok (string= "hi" (w:submit-event-text (first events))))
        (ok (eq input (w:widget-event-widget (first events))) "the event names its widget"))
      (ok (eq input (h:tui-stop tui)) "the final model is the widget"))))

(deftest widget-program-quit-on
  (let ((input (make-instance 'w:text-input)))
    (with-widget-tui (tui program input :cols 20 :rows 3 :quit-on '(w:submit-event))
      (ok (equal '(w:submit-event) (w:widget-program-quit-on program)))
      (h:tui-type tui "go")
      (h:tui-press tui :enter)
      (ok (eq input (h:tui-result tui :timeout 3)) "quits by itself on a submit-event")
      (ok (not (h:tui-running-p tui)))
      (ok (equal '("go") (mapcar #'w:submit-event-text (w:widget-program-events program)))))))

(deftest widget-program-init-command
  (let* ((spinner (make-instance 'w:spinner :interval 0.25))
         (program (w:make-widget-program spinner)))
    (multiple-value-bind (model command) (tui:tui-init program)
      (ok (eq spinner model))
      (ok (equal (tui:tick 0.25) command)))))
