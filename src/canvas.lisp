(in-package #:tui-protocol)

;;; A canvas is a grid of terminal-protocol cells. A sub-canvas is a clipped
;;; window onto its parent's storage: drawing at (0, 0) in it lands at its
;;; origin in the parent, and nothing escapes its rectangle.

(defstruct (canvas (:constructor %make-canvas))
  (cols 0 :type (integer 0))
  (rows 0 :type (integer 0))
  (store nil :type simple-vector)       ; shared row-major cells of the root
  (stride 0 :type (integer 0))          ; root width
  (x0 0 :type (integer 0))
  (y0 0 :type (integer 0)))

(defun make-canvas (cols rows)
  "A COLS × ROWS canvas of blank cells."
  (check-type cols (integer 0))
  (check-type rows (integer 0))
  (%make-canvas :cols cols :rows rows :stride cols
                :store (make-array (* cols rows) :initial-element (tp:make-cell))))

(defun sub-canvas (canvas x y w h)
  "A view of CANVAS's rectangle at X, Y of size W × H, clipped to CANVAS."
  (let* ((x (max 0 (min x (canvas-cols canvas))))
         (y (max 0 (min y (canvas-rows canvas))))
         (w (max 0 (min w (- (canvas-cols canvas) x))))
         (h (max 0 (min h (- (canvas-rows canvas) y)))))
    (%make-canvas :cols w :rows h :store (canvas-store canvas)
                  :stride (canvas-stride canvas)
                  :x0 (+ (canvas-x0 canvas) x) :y0 (+ (canvas-y0 canvas) y))))

(defun %index (canvas x y)
  (+ (* (+ y (canvas-y0 canvas)) (canvas-stride canvas)) (canvas-x0 canvas) x))

(defun canvas-cell (canvas x y)
  "The cell at X, Y (a TERMINAL-PROTOCOL:CELL)."
  (aref (canvas-store canvas) (%index canvas x y)))

(defun (setf canvas-cell) (cell canvas x y)
  (when (and (< -1 x (canvas-cols canvas)) (< -1 y (canvas-rows canvas)))
    (setf (aref (canvas-store canvas) (%index canvas x y)) cell)))

(defun %put (canvas x y cell)
  ;; Overwriting half of a wide character blanks the other half.
  (when (and (< -1 x (canvas-cols canvas)) (< -1 y (canvas-rows canvas)))
    (let ((old (canvas-cell canvas x y)))
      (case (tp:cell-width old)
        (:wide (when (< (1+ x) (canvas-cols canvas))
                 (setf (canvas-cell canvas (1+ x) y)
                       (tp:make-cell :style (tp:cell-style old)))))
        (:spacer (when (> x 0)
                   (setf (canvas-cell canvas (1- x) y)
                         (tp:make-cell :style (tp:cell-style old)))))))
    (setf (canvas-cell canvas x y) cell)))

(defun clear-canvas (canvas &key (style tp:+default-style+))
  (fill-rect canvas 0 0 (canvas-cols canvas) (canvas-rows canvas) :style style))

(defun fill-rect (canvas x y w h &key (char #\Space) (style tp:+default-style+))
  "Fill a rectangle with CHAR in STYLE (clipped). With the default #\\Space the
cells are blank (erased), which a terminal shows as STYLE's background."
  (let ((cell (tp:make-cell :text (if (char= char #\Space) "" (string char)) :style style)))
    (loop for yy from (max 0 y) below (min (+ y h) (canvas-rows canvas))
          do (loop for xx from (max 0 x) below (min (+ x w) (canvas-cols canvas))
                   do (%put canvas xx yy cell))))
  canvas)

(defun draw-text (canvas x y string &key (style tp:+default-style+))
  "Draw STRING starting at X, Y on one line, clipped to the canvas. Wide
characters take two cells, combining marks join the previous cell, and a
wide character that would be cut in half becomes a blank. → the column
after the text."
  (let ((last-x nil))
    (loop for ch across string
          for w = (tp:char-width ch)
          do (cond ((zerop w)
                    (when (and last-x (< -1 last-x (canvas-cols canvas))
                               (< -1 y (canvas-rows canvas)))
                      (let ((c (canvas-cell canvas last-x y)))
                        (setf (canvas-cell canvas last-x y)
                              (tp:make-cell :text (concatenate 'string (tp:cell-text c)
                                                               (string ch))
                                            :width (tp:cell-width c)
                                            :style (tp:cell-style c))))))
                   ((= w 2)
                    (if (< (1+ x) (canvas-cols canvas))
                        (progn (%put canvas x y (tp:make-cell :text (string ch) :width :wide
                                                              :style style))
                               (%put canvas (1+ x) y (tp:make-cell :width :spacer :style style)))
                        (%put canvas x y (tp:make-cell :style style)))
                    (setf last-x x)
                    (incf x 2))
                   (t (%put canvas x y (tp:make-cell :text (string ch) :style style))
                      (setf last-x x)
                      (incf x 1))))
    x))

(defparameter +box-light+ '(#\┌ #\─ #\┐ #\│ #\└ #\┘))

(defun draw-box (canvas x y w h &key (style tp:+default-style+) title)
  "Draw a light box outline. TITLE, if given, sits in the top edge."
  (when (and (>= w 2) (>= h 2))
    (destructuring-bind (tl hz tr vt bl br) +box-light+
      (draw-text canvas x y (string tl) :style style)
      (fill-rect canvas (1+ x) y (- w 2) 1 :char hz :style style)
      (draw-text canvas (+ x w -1) y (string tr) :style style)
      (loop for yy from (1+ y) below (+ y h -1)
            do (draw-text canvas x yy (string vt) :style style)
               (draw-text canvas (+ x w -1) yy (string vt) :style style))
      (draw-text canvas x (+ y h -1) (string bl) :style style)
      (fill-rect canvas (1+ x) (+ y h -1) (- w 2) 1 :char hz :style style)
      (draw-text canvas (+ x w -1) (+ y h -1) (string br) :style style)
      (when (and title (> w 4))
        (draw-text (sub-canvas canvas (+ x 2) y (- w 4) 1) 0 0 title :style style))))
  canvas)

(defun canvas-lines (canvas)
  "One string per row, trailing blanks trimmed (like TERMINAL-SNAPSHOT :LINES)."
  (loop for y below (canvas-rows canvas)
        collect (string-right-trim
                 " "
                 (with-output-to-string (s)
                   (dotimes (x (canvas-cols canvas))
                     (let ((c (canvas-cell canvas x y)))
                       (unless (eq (tp:cell-width c) :spacer)
                         (write-string (if (string= (tp:cell-text c) "") " " (tp:cell-text c))
                                       s))))))))
