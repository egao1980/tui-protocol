(in-package #:tui-protocol/tests)

;;; A scripted in-memory backend: input chunks are handed out one per read;
;;; output goes to an emulator so the test can look at the screen.

(defclass scripted-backend (tui:tui-backend)
  ((input :initarg :input :accessor input)
   (term :initarg :term :reader term)
   (started :initform nil :accessor started)
   (stopped :initform nil :accessor stopped)))

(defmethod tui:backend-start ((b scripted-backend) &key alt-screen mouse)
  (declare (ignore alt-screen mouse))
  (setf (started b) t)
  (tp:terminal-size (term b)))
(defmethod tui:backend-stop ((b scripted-backend)) (setf (stopped b) t))
(defmethod tui:backend-size ((b scripted-backend)) (tp:terminal-size (term b)))
(defmethod tui:backend-write ((b scripted-backend) octets) (tp:terminal-feed (term b) octets))
(defmethod tui:backend-read ((b scripted-backend) &key timeout)
  (declare (ignore timeout))
  (if (input b) (octets (pop (input b))) nil))

(defclass counter () ())
(defmethod tui:tui-init ((p counter)) (values 0 nil))
(defmethod tui:tui-update ((p counter) model (m tui:key-message))
  (case (tui:key-message-key m)
    (:equal (values (1+ model) nil))         ; "+" is Shift+Equal
    (:minus (values (1- model) nil))
    (:q (values model :quit))
    (t (values model nil))))
(defmethod tui:tui-view ((p counter) model canvas)
  (tui:draw-box canvas 0 0 (tui:canvas-cols canvas) 3 :title "counter")
  (tui:draw-text canvas 2 1 (format nil "count: ~D" model)))

(deftest counter-program-runs-and-restores
  (with-emulator (term 20 4)
    (let* ((backend (make-instance 'scripted-backend :term term :input (list "++" "+" "-" "q"))))
      (ok (= 2 (tui:run-tui (make-instance 'counter) :backend backend :alt-screen nil)))
      (ok (and (started backend) (stopped backend)))
      ;; The last frame drawn is on screen.
      (ok (equal (second (getf (tp:terminal-snapshot term) :lines)) "│ count: 2         │"))
      ;; Teardown turned bracketed paste back off.
      (ng (tp:terminal-mode term :bracketed-paste)))))

(defclass ticking () ())
(defmethod tui:tui-init ((p ticking)) (values 0 (tui:tick 0)))
(defmethod tui:tui-update ((p ticking) model (m tui:tick-message))
  (if (>= model 2) (values model :quit) (values (1+ model) (tui:tick 0))))
(defmethod tui:tui-update ((p ticking) model (m tui:message)) (values model nil))
(defmethod tui:tui-view ((p ticking) model canvas) (tui:draw-text canvas 0 0 (princ-to-string model)))

(deftest ticks-and-tasks
  (with-emulator (term 10 2)
    (let ((backend (make-instance 'scripted-backend :term term
                                                    :input (make-list 50 :initial-element ""))))
      (ok (= 2 (tui:run-tui (make-instance 'ticking) :backend backend :alt-screen nil))))))

(defclass crashing () ())
(defmethod tui:tui-view ((p crashing) model canvas) (declare (ignore model canvas)))
(defmethod tui:tui-update ((p crashing) model m) (declare (ignore model m)) (error "boom"))

(deftest terminal-restored-on-error
  (with-emulator (term 10 2)
    (let ((backend (make-instance 'scripted-backend :term term :input (list "x"))))
      (ok (signals (tui:run-tui (make-instance 'crashing) :backend backend :alt-screen nil)
                   'simple-error))
      (ok (stopped backend))
      (ng (tp:terminal-mode term :bracketed-paste)))))
