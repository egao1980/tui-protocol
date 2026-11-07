(in-package #:tui-protocol/tests)

;;; The renderer is checked by a real emulator: its output, fed to a
;;; terminal-protocol terminal, must reproduce the canvas cell for cell.

(defun screen-matches-canvas-p (term canvas)
  (let ((cells (getf (tp:terminal-snapshot term) :cells)))
    (loop for y below (tui:canvas-rows canvas)
          always (loop for x below (tui:canvas-cols canvas)
                       always (equalp (aref (aref cells y) x) (tui:canvas-cell canvas x y))))))

(defun scene-1 ()
  (let ((c (tui:make-canvas 20 4)))
    (tui:draw-box c 0 0 20 4 :title "menu" :style (tp:make-style :foreground (tp:make-rgb 9 8 7)))  ; rgb: the emulator resolves palette indexes
    (tui:draw-text c 2 1 "open" :style (tp:make-style :bold t :inverse t))
    (tui:draw-text c 2 2 "日本 close" :style (tp:make-style :foreground (tp:make-rgb 1 2 3)
                                                             :underline :curly))
    c))

(deftest full-render-reproduces-canvas
  (with-emulator (term 20 4)
    (let ((c (scene-1)))
      (tp:terminal-feed term (tui:render-full c))
      (ok (equal (getf (tp:terminal-snapshot term) :lines) (tui:canvas-lines c)))
      (ok (screen-matches-canvas-p term c)))))

(deftest diff-render-reproduces-canvas
  (with-emulator (term 20 4)
    (let ((a (scene-1)) (b (scene-1)))
      (tp:terminal-feed term (tui:render-full a))
      (tui:draw-text b 2 1 "save" :style (tp:make-style :italic t))
      (tui:draw-text b 15 2 "!!")
      (let ((bytes (tui:render-diff a b)))
        (ok (< (length bytes) (length (tui:render-full b))))
        (tp:terminal-feed term bytes))
      (ok (screen-matches-canvas-p term b))
      (ok (equalp (tui:render-diff b b)
                  (tui:render-diff b b)))))) ; deterministic

(deftest hyperlinks-and-palette
  (with-emulator (term 10 1)
    (let ((c (tui:make-canvas 10 1)))
      (tui:draw-text c 0 0 "link" :style (tp:make-style :hyperlink "https://x.test/" :foreground 9))
      (tui:draw-text c 5 0 "bg" :style (tp:make-style :background 200))
      (tp:terminal-feed term (tui:render-full c))
      (let ((row (aref (getf (tp:terminal-snapshot term) :cells) 0)))
        (ok (equal (tp:style-hyperlink (tp:cell-style (aref row 0))) "https://x.test/"))
        (ng (tp:style-hyperlink (tp:cell-style (aref row 5))))
        ;; The emulator resolves palette colours; the renderer sent index 9 and 200.
        (ok (tp:rgb-p (tp:style-foreground (tp:cell-style (aref row 0)))))
        (ok (tp:rgb-p (tp:style-background (tp:cell-style (aref row 5)))))))))

(deftest synchronized-and-cursor
  (let* ((c (tui:make-canvas 4 1))
         (bytes (map 'string #'code-char (tui:render-full c :synchronized t :cursor '(2 . 0)))))
    (ok (search (format nil "~C[?2026h" #\Esc) bytes))
    (ok (search (format nil "~C[1;3H~C[?25h" #\Esc #\Esc) bytes))))
