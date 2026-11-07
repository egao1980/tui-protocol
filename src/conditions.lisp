(in-package #:tui-protocol)

(define-condition tui-error (error) ()
  (:report (lambda (c s) (declare (ignore c)) (format s "TUI error"))))

(define-condition tui-backend-error (tui-error)
  ((cause :initarg :cause :initform nil :reader tui-backend-error-cause))
  (:report (lambda (c s)
             (format s "TUI backend failed~@[: ~A~]" (tui-backend-error-cause c)))))
