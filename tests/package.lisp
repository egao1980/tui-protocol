(defpackage #:tui-protocol/tests
  (:use #:cl #:rove)
  (:local-nicknames (#:tui #:tui-protocol) (#:tp #:terminal-protocol)
                    (#:tl #:terminal-backend-lisp)))
(in-package #:tui-protocol/tests)

(defun octets (s)
  (let ((out (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (tui-protocol::%emit-utf8 s out)
    (coerce out '(simple-array (unsigned-byte 8) (*)))))

(defmacro with-emulator ((var cols rows) &body body)
  `(tp:with-terminal (,var :backend (make-instance 'tl:lisp-backend) :cols ,cols :rows ,rows)
     ,@body))
