(in-package #:tui-protocol/widgets)

;;; ------------------------------------------------------------------
;;; Styled text runs. A LINE is a list of TEXT-RUNs; a renderer (markdown,
;;; diff) emits lines, widgets draw them. Strings stand for one default run.

(defstruct (text-run (:constructor %make-text-run (text style)))
  (text "" :type string)
  (style tp:+default-style+ :type tp:style))

(defun text-run (text &key style bold dim italic underline inverse foreground background)
  "A run of TEXT in a style. STYLE (default TP:+DEFAULT-STYLE+) is the base;
the other keys override it when non-NIL: BOLD, DIM (= faint), ITALIC,
INVERSE are booleans, UNDERLINE is T (= :SINGLE) or a TP underline kind,
FOREGROUND / BACKGROUND are TP:COLORs. The base style is never mutated.
TEXT must be a string (TYPE-ERROR otherwise)."
  (check-type text string)
  (check-type style (or null tp:style))
  (let ((base (or style tp:+default-style+)))
    (if (or bold dim italic underline inverse foreground background)
        (let ((s (tp:copy-style base)))
          (when bold (setf (tp:style-bold s) t))
          (when dim (setf (tp:style-faint s) t))
          (when italic (setf (tp:style-italic s) t))
          (when inverse (setf (tp:style-inverse s) t))
          (when underline
            (setf (tp:style-underline s) (if (eq underline t) :single underline)))
          (when foreground (setf (tp:style-foreground s) foreground))
          (when background (setf (tp:style-background s) background))
          (%make-text-run text s))
        (%make-text-run text base))))

(defun as-runs (line)
  "LINE as a list of runs: a string → one default-style run (\"\" → ()), a
TEXT-RUN → a list of it, a list of runs → itself."
  (etypecase line
    (string (if (zerop (length line))
                '()
                (list (%make-text-run line tp:+default-style+))))
    (text-run (list line))
    (list (if (every #'text-run-p line)
              line
              (loop for part in line append (as-runs part))))))

(defun runs-string (runs)
  "The concatenated text of RUNS (anything AS-RUNS takes)."
  (with-output-to-string (out)
    (dolist (run (as-runs runs))
      (write-string (text-run-text run) out))))

(defun runs-width (runs)
  "Display width of RUNS in cells (TP:STRING-WIDTH: wide = 2, combining = 0)."
  (loop for run in (as-runs runs) sum (tp:string-width (text-run-text run))))

(defun draw-runs (canvas x y runs)
  "Draw RUNS on one row of CANVAS from X, each in its style, clipped like
TUI:DRAW-TEXT. → the column after the text."
  (dolist (run (as-runs runs) x)
    (setf x (tui:draw-text canvas x y (text-run-text run) :style (text-run-style run)))))

;;; ------------------------------------------------------------------
;;; Clusters: a base character plus the zero-width characters after it.
;;; Shared by wrapping, truncation and text-input's cursor motion and rows.
;;; Note TP:CHAR-WIDTH of #\Newline is 1: split hard lines before measuring.

(declaim (inline %mark-p))
(defun %mark-p (char)
  (zerop (tp:char-width char)))

(defun %cluster-end (string start &optional (end (length string)))
  "Index after the cluster that starts at START (< END)."
  (let ((j (1+ start)))
    (loop while (and (< j end) (%mark-p (char string j))) do (incf j))
    (min j end)))

(defun %cluster-start (string index)
  "The cluster boundary before INDEX (> 0)."
  (let ((j (1- index)))
    (loop while (and (> j 0) (%mark-p (char string j))) do (decf j))
    (max j 0)))

(defun %fit-end (string start end limit)
  "Largest cluster boundary E in [START, END] with width of STRING[START, E) ≤ LIMIT."
  (let ((w 0) (i start))
    (loop while (< i end)
          do (let* ((e (%cluster-end string i end))
                    (cw (tp:string-width string :start i :end e)))
               (when (> (+ w cw) limit) (return))
               (incf w cw)
               (setf i e)))
    i))

(defun %break-ranges (string start end width &key (words t))
  "Visual rows of the hard line STRING[START, END) at WIDTH cells, as a list of
(row-start . row-end). WORDS NIL breaks by cells only (rule 3). An empty line
is one empty row."
  (if (= start end)
      (list (cons start start))
      (let ((pos start) (rows '()))
        (loop while (< pos end)
              do (let ((e (%fit-end string pos end width)))
                   (if (= e end)
                       (progn (push (cons pos end) rows) (setf pos end))
                       (let ((space (and words
                                         (< (1+ pos) (min end (1+ e)))
                                         (position #\Space string :start (1+ pos)
                                                                  :end (min end (1+ e))
                                                                  :from-end t))))
                         (if space
                             (progn (push (cons pos space) rows) (setf pos (1+ space)))
                             (progn (when (= e pos) (setf e (%cluster-end string pos end)))
                                    (push (cons pos e) rows)
                                    (setf pos e)))))))
        (nreverse rows))))

(defun %slice (runs start end)
  "The runs covering [START, END) of (RUNS-STRING RUNS): split, never merged,
no empty runs."
  (loop with pos = 0
        for run in runs
        for len = (length (text-run-text run))
        for s = (max start pos)
        for e = (min end (+ pos len))
        when (< s e)
          collect (if (and (= s pos) (= e (+ pos len)))
                      run
                      (%make-text-run (subseq (text-run-text run) (- s pos) (- e pos))
                                      (text-run-style run)))
        do (incf pos len)))

(defun wrap-runs (runs width)
  "Word-wrap RUNS into visual lines of at most WIDTH cells (an integer ≥ 1).
→ a list of lines, each a list of runs with the original styles.

1. #\\Newline splits hard lines; an empty hard line is one empty line ().
2. A hard line that fits is one line. Otherwise break at the last space
   (U+0020) whose non-empty prefix fits; that one space is dropped.
3. With no such space, break after the longest prefix of whole characters
   that fits (a combining mark stays with its base; a wide character that
   does not fit moves to the next line). A line always takes at least one
   character.
Runs are split at breaks, never merged; empty runs are left out."
  (check-type width (integer 1))
  (let* ((runs (as-runs runs))
         (s (runs-string runs))
         (n (length s)))
    (loop for a = 0 then (1+ b)
          for b = (or (position #\Newline s :start a) n)
          nconc (if (= a b)
                    (list '())
                    (loop for (x . y) in (%break-ranges s a b width)
                          collect (%slice runs x y)))
          while (< b n))))

(defun truncate-runs (runs width &key (ellipsis "…"))
  "RUNS cut to at most WIDTH cells. When cut, the result ends with ELLIPSIS
(in the style of the last kept run) and the whole is still ≤ WIDTH."
  (check-type width (integer 0))
  (check-type ellipsis string)
  (let ((runs (as-runs runs)))
    (if (<= (runs-width runs) width)
        runs
        (let* ((s (runs-string runs))
               (n (length s))
               (ew (tp:string-width ellipsis)))
          (if (<= ew width)
              (let* ((kept (%slice runs 0 (%fit-end s 0 n (- width ew))))
                     (style (text-run-style (or (car (last kept)) (first runs)))))
                (if (zerop (length ellipsis))
                    kept
                    (append kept (list (%make-text-run ellipsis style)))))
              (%slice runs 0 (%fit-end s 0 n width)))))))
