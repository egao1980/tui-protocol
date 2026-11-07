(defsystem "tui-protocol"
  :version "0.1.0"
  :description "CLOS terminal user interface protocol for cl-stack: init/update/view over a canvas of terminal-protocol cells, diff renderer, input decoder"
  :author "egao1980"
  :license "MIT"
  :depends-on ("terminal-protocol" "bordeaux-threads")
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "conditions")
               (:file "canvas")
               (:file "render")
               (:file "messages")
               (:file "input")
               (:file "program"))
  :in-order-to ((test-op (test-op "tui-protocol/tests"))))

(defsystem "tui-protocol/tests"
  :depends-on ("tui-protocol" "terminal-backend-lisp" "rove")
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "canvas-test")
               (:file "render-test")
               (:file "input-test")
               (:file "program-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))
