(defsystem "tui-protocol"
  :version "0.2.0"
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
  :in-order-to ((test-op (test-op "tui-protocol/tests" "tui-protocol/widgets/tests"))))

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

(defsystem "tui-protocol/widgets"
  :version "0.2.0"
  :description "Widgets for tui-protocol programs: multi-line text input, scrolling viewport, select list, confirm dialog, spinner, status line, styled text runs"
  :author "egao1980"
  :license "MIT"
  :depends-on ("tui-protocol" "terminal-protocol")
  :serial t
  :pathname "widgets"
  :components ((:file "package")
               (:file "runs")
               (:file "widget")
               (:file "text-input")
               (:file "viewport")
               (:file "choices")
               (:file "indicators"))
  :in-order-to ((test-op (test-op "tui-protocol/widgets/tests"))))

(defsystem "tui-protocol/widgets/tests"
  :depends-on ("tui-protocol/widgets" "tui-backend-terminal" "terminal-backend-lisp" "rove")
  :pathname "tests/widgets"
  :serial t
  :components ((:file "package")
               (:file "runs-test")
               (:file "program-test")
               (:file "text-input-test")
               (:file "viewport-test")
               (:file "choices-test")
               (:file "indicators-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))
