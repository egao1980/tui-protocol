(in-package #:tui-protocol)

;;; ------------------------------------------------------------------
;;; Program

(defgeneric tui-init (program)
  (:documentation "→ (values model command). COMMAND may be NIL.")
  (:method (program) (declare (ignore program)) (values nil nil)))

(defgeneric tui-update (program model message)
  (:documentation "→ (values model command) after MESSAGE.")
  (:method (program model message)
    (declare (ignore program message))
    (values model nil)))

(defgeneric tui-view (program model canvas)
  (:documentation "Draw MODEL on CANVAS, a fresh blank canvas the size of the
terminal. The return value is ignored."))

;;; Commands: :QUIT, (TICK seconds), (TASK function), (BATCH command …).

(defun tick (seconds)
  "Command: deliver a TICK-MESSAGE after SECONDS."
  (check-type seconds (real 0))
  (list :tick seconds))

(defun task (function)
  "Command: call FUNCTION on another thread; a non-NIL return value is sent
back as a message."
  (check-type function function)
  (list :task function))

(defun batch (&rest commands)
  (cons :batch (remove nil commands)))

;;; ------------------------------------------------------------------
;;; Backend

(defclass tui-backend () ()
  (:documentation "Where a TUI runs: bytes in, bytes out, a size."))

(defvar *tui-backend* nil)

(defgeneric backend-start (backend &key alt-screen mouse)
  (:documentation "Take over the terminal (raw mode, alt screen, mouse, paste,
focus). → (values cols rows)."))
(defgeneric backend-stop (backend)
  (:documentation "Give the terminal back exactly as it was."))
(defgeneric backend-read (backend &key timeout)
  (:documentation "→ octets the terminal sent, empty on timeout, NIL when input is gone."))
(defgeneric backend-write (backend octets))
(defgeneric backend-size (backend)
  (:documentation "→ (values cols rows), current."))

(defun %csi-private (n set-p)
  (format nil "~C[?~D~:[l~;h~]" #\Esc n set-p))

(defun %setup-sequence (alt-screen mouse)
  (apply #'concatenate 'string
         (append (when alt-screen (list (%csi-private 1049 t)))
                 (list (%csi-private 2004 t) (%csi-private 1004 t))
                 (when mouse (list (%csi-private 1002 t) (%csi-private 1006 t))))))

(defun %teardown-sequence (alt-screen mouse)
  (apply #'concatenate 'string
         (format nil "~C[0m" #\Esc)
         (append (when mouse (list (%csi-private 1006 nil) (%csi-private 1002 nil)))
                 (list (%csi-private 1004 nil) (%csi-private 2004 nil) (%csi-private 25 t))
                 (when alt-screen (list (%csi-private 1049 nil))))))

;;; ------------------------------------------------------------------
;;; Loop

(defstruct (tui-run (:constructor %make-tui-run))
  program backend
  (queue '()) (lock (bt2:make-lock :name "tui-queue"))
  (timers '())         ; (due-internal-time . seconds)
  (quit nil))

(defvar *tui* nil "The running TUI, inside TUI-UPDATE / TUI-VIEW and tasks it starts.")

(defun tui-send (message &optional (run *tui*))
  "Queue MESSAGE for the running TUI (safe from any thread)."
  (bt2:with-lock-held ((tui-run-lock run))
    (setf (tui-run-queue run) (append (tui-run-queue run) (list message))))
  message)

(defun %run-command (run command)
  (cond ((null command))
        ((eq command :quit) (setf (tui-run-quit run) t))
        ((eq (first command) :tick)
         (push (cons (+ (get-internal-real-time)
                        (round (* (second command) internal-time-units-per-second)))
                     (second command))
               (tui-run-timers run)))
        ((eq (first command) :task)
         (let ((function (second command)))
           (bt2:make-thread (lambda ()
                              (let ((result (let ((*tui* run)) (funcall function))))
                                (when result (tui-send result run))))
                            :name "tui-task")))
        ((eq (first command) :batch)
         (dolist (c (rest command)) (%run-command run c)))
        (t (error 'type-error :datum command :expected-type 'tui-command))))

(defun %due-ticks (run)
  (let ((now (get-internal-real-time)) (due '()))
    (setf (tui-run-timers run)
          (remove-if (lambda (timer)
                       (when (<= (car timer) now)
                         (push (make-tick-message :time (/ now internal-time-units-per-second))
                               due)
                         t))
                     (tui-run-timers run)))
    (nreverse due)))

(defun %next-timeout (run)
  (let ((timers (tui-run-timers run)))
    (if timers
        (max 0 (/ (- (reduce #'min timers :key #'car) (get-internal-real-time))
                  internal-time-units-per-second))
        0.05)))

(defun run-tui (program &key (backend *tui-backend*) (alt-screen t) mouse (synchronized t))
  "Run PROGRAM on BACKEND until a command returns :QUIT or input ends.
The terminal is restored however the loop exits. → the final model."
  (unless backend (error 'tui-backend-error :cause "*TUI-BACKEND* is NIL"))
  (let ((run (%make-tui-run :program program :backend backend))
        (decoder (make-input-decoder))
        (model nil) (screen nil) (cols 80) (rows 24))
    (let ((*tui* run))
      (unwind-protect
           (progn
             (multiple-value-setq (cols rows) (backend-start backend :alt-screen alt-screen
                                                                     :mouse mouse))
             (backend-write backend (%utf8-octets (%setup-sequence alt-screen mouse)))
             (multiple-value-bind (m command) (tui-init program)
               (setf model m)
               (%run-command run command))
             (loop
               ;; Draw.
               (let ((canvas (make-canvas cols rows)))
                 (tui-view program model canvas)
                 (backend-write backend (render-diff screen canvas :synchronized synchronized))
                 (setf screen canvas))
               (when (tui-run-quit run) (return))
               ;; Gather messages: input, size changes, ticks, sent messages.
               (let* ((input (backend-read backend :timeout (%next-timeout run)))
                      (messages (cond ((null input) (setf (tui-run-quit run) t) '())
                                      ((plusp (length input)) (decode-input decoder input))
                                      (t (flush-input decoder)))))
                 (multiple-value-bind (c r) (backend-size backend)
                   (unless (and (= c cols) (= r rows))
                     (setf cols c rows r screen nil)
                     (push (make-resize-message :cols c :rows r) messages)))
                 (setf messages (append messages (%due-ticks run)
                                        (bt2:with-lock-held ((tui-run-lock run))
                                          (shiftf (tui-run-queue run) '()))))
                 (dolist (message messages)
                   (multiple-value-bind (m command) (tui-update program model message)
                     (setf model m)
                     (%run-command run command)))))
             model)
        ;; Cleanup may run while an error unwinds; a failing write here must not
        ;; replace that error, and the backend must still be stopped.
        (handler-case
            (backend-write backend (%utf8-octets (%teardown-sequence alt-screen mouse)))
          (error () nil))
        (backend-stop backend)))))

(defun %utf8-octets (string)
  (let ((out (make-array (length string) :element-type '(unsigned-byte 8)
                                          :adjustable t :fill-pointer 0)))
    (%emit-utf8 string out)
    (coerce out '(simple-array (unsigned-byte 8) (*)))))
