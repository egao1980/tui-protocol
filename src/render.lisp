(in-package #:tui-protocol)

;;; Canvas → VT bytes. RENDER-DIFF compares the previous frame with the next
;;; and writes only changed cells: a cursor move when it jumps, an SGR when
;;; the style changes, OSC 8 around links. The output is checked by feeding it
;;; through a real terminal-protocol emulator (see tests).

(defun %color-params (color base bright extended)
  "SGR parameters for COLOR as foreground (BASE 30) or background (BASE 40)."
  (cond ((eq color :default) (list (+ base 9)))
        ((integerp color)
         (cond ((< color 8) (list (+ base color)))
               ((< color 16) (list (+ bright (- color 8))))
               (t (list extended 5 color))))
        (t (list extended 2 (tp:rgb-r color) (tp:rgb-g color) (tp:rgb-b color)))))

(defun %sgr (style)
  "A complete SGR for STYLE, starting from a reset."
  (let ((params (list 0)))
    (flet ((add (&rest ps) (setf params (append params ps))))
      (when (tp:style-bold style) (add 1))
      (when (tp:style-faint style) (add 2))
      (when (tp:style-italic style) (add 3))
      (case (tp:style-underline style)
        (:single (add 4)) (:double (add "4:2")) (:curly (add "4:3"))
        (:dotted (add "4:4")) (:dashed (add "4:5")))
      (when (tp:style-blink style) (add 5))
      (when (tp:style-inverse style) (add 7))
      (when (tp:style-invisible style) (add 8))
      (when (tp:style-strikethrough style) (add 9))
      (when (tp:style-overline style) (add 53))
      (unless (eq (tp:style-foreground style) :default)
        (apply #'add (%color-params (tp:style-foreground style) 30 90 38)))
      (unless (eq (tp:style-background style) :default)
        (apply #'add (%color-params (tp:style-background style) 40 100 48)))
      (let ((u (tp:style-underline-color style)))
        (unless (eq u :default)
          (apply #'add (if (integerp u) (list 58 5 u)
                           (list 58 2 (tp:rgb-r u) (tp:rgb-g u) (tp:rgb-b u)))))))
    (format nil "~C[~{~A~^;~}m" #\Esc params)))

(defun %emit-utf8 (string out)
  (loop for ch across string
        for code = (char-code ch)
        do (cond ((< code #x80) (vector-push-extend code out))
                 ((< code #x800)
                  (vector-push-extend (logior #xc0 (ash code -6)) out)
                  (vector-push-extend (logior #x80 (logand code #x3f)) out))
                 ((< code #x10000)
                  (vector-push-extend (logior #xe0 (ash code -12)) out)
                  (vector-push-extend (logior #x80 (logand (ash code -6) #x3f)) out)
                  (vector-push-extend (logior #x80 (logand code #x3f)) out))
                 (t
                  (vector-push-extend (logior #xf0 (ash code -18)) out)
                  (vector-push-extend (logior #x80 (logand (ash code -12) #x3f)) out)
                  (vector-push-extend (logior #x80 (logand (ash code -6) #x3f)) out)
                  (vector-push-extend (logior #x80 (logand code #x3f)) out)))))

(defun %render (old new full synchronized cursor)
  (let ((out (make-array 256 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
        (style nil) (link nil) (px -1) (py -1))
    (labels ((emit (s) (%emit-utf8 s out))
             (move (x y)
               (unless (and (= x px) (= y py))
                 (emit (format nil "~C[~D;~DH" #\Esc (1+ y) (1+ x))))
               (setf px x py y))
             (set-style (s)
               (unless (and style (equalp s style))
                 (emit (%sgr s))
                 (setf style s))
               (let ((l (tp:style-hyperlink s)))
                 (unless (equal l link)
                   (emit (format nil "~C]8;;~A~C\\" #\Esc (or l "") #\Esc))
                   (setf link l)))))
      (when synchronized (emit (format nil "~C[?2026h" #\Esc)))
      (emit (format nil "~C[?25l" #\Esc))
      (when full (emit (format nil "~C[0m~C[2J" #\Esc #\Esc)))
      (dotimes (y (canvas-rows new))
        (let ((x 0) (cols (canvas-cols new)))
          (flet ((changed-p (x)
                   (or full
                       (not (equalp (canvas-cell new x y) (canvas-cell old x y)))
                       ;; A changed spacer means the wide cell before it changed too.
                       (and (< (1+ x) cols)
                            (not (equalp (canvas-cell new (1+ x) y)
                                         (canvas-cell old (1+ x) y))))))
                 (blank-p (cell)
                   (and (string= (tp:cell-text cell) "") (eq (tp:cell-width cell) :narrow))))
            (loop while (< x cols)
                  do (let ((cell (canvas-cell new x y)))
                       (cond
                         ((or (eq (tp:cell-width cell) :spacer) (not (changed-p x)))
                          (incf x))
                         ;; A run of blank cells in one style: erase it (ECH), which
                         ;; leaves exactly those cells blank in that background.
                         ((blank-p cell)
                          (let ((end (1+ x)))
                            (loop while (and (< end cols)
                                             (blank-p (canvas-cell new end y))
                                             (equalp (tp:cell-style (canvas-cell new end y))
                                                     (tp:cell-style cell))
                                             (changed-p end))
                                  do (incf end))
                            (move x y)
                            (set-style (tp:cell-style cell))
                            (emit (format nil "~C[~DX" #\Esc (- end x)))
                            (setf x end)))
                         (t
                          (move x y)
                          (set-style (tp:cell-style cell))
                          (emit (tp:cell-text cell))
                          (incf px (if (eq (tp:cell-width cell) :wide) 2 1))
                          ;; Pending wrap at the last column: force an explicit move next.
                          (when (>= px cols) (setf px -1))
                          (incf x (if (eq (tp:cell-width cell) :wide) 2 1)))))))))
      (when link (emit (format nil "~C]8;;~C\\" #\Esc #\Esc)))
      (emit (format nil "~C[0m" #\Esc))
      (when cursor
        (emit (format nil "~C[~D;~DH~C[?25h" #\Esc (1+ (cdr cursor)) (1+ (car cursor)) #\Esc)))
      (when synchronized (emit (format nil "~C[?2026l" #\Esc)))
      (coerce out '(simple-array (unsigned-byte 8) (*))))))

(defun render-full (canvas &key synchronized cursor)
  "Bytes that clear the screen and draw CANVAS. CURSOR is (x . y) to show the
cursor there, or NIL to leave it hidden. SYNCHRONIZED wraps the frame in
DEC 2026 synchronized output."
  (%render nil canvas t synchronized cursor))

(defun render-diff (old new &key synchronized cursor)
  "Bytes that turn the screen showing OLD into NEW. Falls back to
RENDER-FULL when OLD is NIL or a different size."
  (if (and old (= (canvas-cols old) (canvas-cols new)) (= (canvas-rows old) (canvas-rows new)))
      (%render old new nil synchronized cursor)
      (render-full new :synchronized synchronized :cursor cursor)))
