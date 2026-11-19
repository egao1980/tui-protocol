(defpackage #:tui-protocol/widgets/tests
  (:use #:cl #:rove)
  (:local-nicknames (#:tui #:tui-protocol) (#:tp #:terminal-protocol)
                    (#:w #:tui-protocol/widgets) (#:h #:tui-backend-terminal)
                    (#:tl #:terminal-backend-lisp)))
(in-package #:tui-protocol/widgets/tests)

;;; Two ways in:
;;;  - model level: build messages, call WIDGET-UPDATE / WIDGET-VIEW on a
;;;    canvas, compare TUI:CANVAS-LINES and cell styles (exact, no threads);
;;;  - end to end: a WIDGET-PROGRAM on tui-backend-terminal's headless driver
;;;    with the pure-Lisp emulator, typed / pasted through the emulator's
;;;    encoders, compared with TUI-SCREEN (one string per row, trailing
;;;    blanks trimmed; a wide character is one string character with no
;;;    spacer, a combining mark follows its base).

(defun u (&rest parts)
  "Concatenate strings, characters and code points: (u \"e\" #x301) → é."
  (with-output-to-string (s)
    (dolist (p parts)
      (etypecase p
        (string (write-string p s))
        (character (write-char p s))
        (integer (write-char (code-char p) s))))))

;;; ------------------------------------------------------------------
;;; Model level

(defun key (name &rest modifiers)
  "A key press without text: (key :enter :alt)."
  (tui:make-key-message :key name :modifiers modifiers))

(defun chars (string)
  "Key messages for typing STRING, as tui-type would send them."
  (loop for ch across string
        collect (multiple-value-bind (k shifted) (tui:char-key ch)
                  (tui:make-key-message :key k :text (string ch)
                                        :modifiers (when shifted '(:shift))))))

(defun paste (string) (tui:make-paste-message :text string))

(defun feed (widget &rest messages)
  "Send MESSAGES (key messages, lists of them, or strings to type) to WIDGET.
→ (values widget events commands): the events and commands returned, in order."
  (let ((events '()) (commands '()))
    (dolist (m messages)
      (dolist (msg (cond ((stringp m) (chars m)) ((listp m) m) (t (list m))))
        (multiple-value-bind (wd command event) (w:widget-update widget msg)
          (setf widget wd)
          (when command (push command commands))
          (when event (push event events)))))
    (values widget (nreverse events) (nreverse commands))))

(defun draw (widget cols rows)
  "A fresh COLS × ROWS canvas with WIDGET drawn on it."
  (let ((canvas (tui:make-canvas cols rows)))
    (w:widget-view widget canvas)
    canvas))

(defun lines (widget cols rows)
  "WIDGET drawn on COLS × ROWS → TUI:CANVAS-LINES (all ROWS rows)."
  (tui:canvas-lines (draw widget cols rows)))

(defun cell-style (canvas x y) (tp:cell-style (tui:canvas-cell canvas x y)))

(defun inverse-cells (canvas)
  "(x y) of every cell with the inverse attribute, row by row."
  (loop for y below (tui:canvas-rows canvas)
        nconc (loop for x below (tui:canvas-cols canvas)
                    when (tp:style-inverse (cell-style canvas x y))
                      collect (list x y))))

(defun pad (rows lines)
  "LINES followed by \"\" up to ROWS rows."
  (append lines (make-list (max 0 (- rows (length lines))) :initial-element "")))

;;; ------------------------------------------------------------------
;;; End to end

(defun emulator () (make-instance 'tl:lisp-backend))

(defmacro with-widget-tui ((tui program widget &key (cols 20) (rows 5) quit-on) &body body)
  "Run WIDGET in a WIDGET-PROGRAM (bound to PROGRAM) on a headless COLS × ROWS
emulator (bound to TUI). The program is stopped afterwards."
  `(let* ((,program (w:make-widget-program ,widget :quit-on ,quit-on))
          (,tui (h:make-headless-tui ,program :cols ,cols :rows ,rows
                                              :terminal-backend (emulator))))
     (unwind-protect (progn ,@body)
       (when (h:tui-running-p ,tui) (ignore-errors (h:tui-stop ,tui))))))

(defun screen-is (tui expected &key (timeout 2))
  "Wait until the screen shows EXPECTED (rows after the given ones blank).
→ T, or NIL on timeout. Signals the program's error if it died."
  (and (h:tui-wait-until
        tui (lambda ()
              (let ((screen (h:tui-screen tui)))
                (equal screen (pad (length screen) expected))))
        :timeout timeout)
       t))

(defun events-reach (program n &key (timeout 2))
  "Wait until PROGRAM has recorded N events. → the events, or NIL."
  (let ((deadline (+ (get-internal-real-time) (* timeout internal-time-units-per-second))))
    (loop
      (let ((events (w:widget-program-events program)))
        (when (>= (length events) n) (return events)))
      (when (> (get-internal-real-time) deadline) (return nil))
      (sleep 0.01))))
