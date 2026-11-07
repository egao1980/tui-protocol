(in-package #:tui-protocol/tests)

(deftest draw-text-and-lines
  (let ((c (tui:make-canvas 10 2)))
    (ok (= 5 (tui:draw-text c 0 0 "hello")))
    (tui:draw-text c 3 1 "x")
    (ok (equal (tui:canvas-lines c) '("hello" "   x")))))

(deftest wide-and-combining
  (let ((c (tui:make-canvas 6 1)))
    (ok (= 6 (tui:draw-text c 0 0 "日本ex")))   ; 2 + 2 + 1 + 1
    (ok (eq :wide (tp:cell-width (tui:canvas-cell c 0 0))))
    (ok (eq :spacer (tp:cell-width (tui:canvas-cell c 1 0))))
    (tui:draw-text c 4 0 (coerce (list #\e (code-char #x301)) 'string))
    (ok (equal (tp:cell-text (tui:canvas-cell c 4 0)) (coerce (list #\e (code-char #x301)) 'string)))))

(deftest clipping-and-half-wide
  (let ((c (tui:make-canvas 3 1)))
    (tui:draw-text c 1 0 "日本")      ; 日 fits at 1-2; 本 would be cut → nothing beyond
    (ok (equal (tui:canvas-lines c) '(" 日")))
    (tui:draw-text c 2 0 "z")         ; overwrites the spacer → the wide cell becomes blank
    (ok (equal (tui:canvas-lines c) '("  z")))))

(deftest sub-canvas-clips
  (let* ((c (tui:make-canvas 8 3))
         (s (tui:sub-canvas c 2 1 3 1)))
    (tui:draw-text s 0 0 "abcdef")
    (ok (equal (tui:canvas-lines c) '("" "  abc" "")))
    (ok (= 3 (tui:canvas-cols s)))))

(deftest boxes
  (let ((c (tui:make-canvas 8 3)))
    (tui:draw-box c 0 0 8 3 :title "hi")
    (ok (equal (tui:canvas-lines c) '("┌─hi───┐" "│      │" "└──────┘")))))
