(in-package #:tui-protocol/widgets)

;;; ------------------------------------------------------------------
;;; text-input: the multi-line prompt editor of a coding-agent CLI.
;;;
;;; The buffer is a list of lines; the cursor is (row, col) where COL is a
;;; character index into the line and always sits on a cluster boundary (a
;;; cluster is a base character plus the zero-width characters after it).
;;; Display columns come from TP:CHAR-WIDTH.
;;;
;;; Keys (focused; :PRESS or :REPEAT):
;;;   text, no :CTRL/:ALT/:SUPER     insert the text
;;;   Enter                          submit; but after a "\" the backslash
;;;                                  becomes a newline; empty buffer: nothing
;;;   Alt+Enter, Shift+Enter,        insert a newline
;;;   Ctrl+Enter, Ctrl+J             (Shift+Enter only reaches us from
;;;                                  terminals that report it: CSI-u)
;;;   Backspace / Delete             delete the cluster before / at the
;;;                                  cursor, joining lines at the edges
;;;   Left / Right                   one cluster, crossing line ends
;;;   Ctrl or Alt + Left / Right,    word left / right (word char:
;;;   Alt+B / Alt+F                  ALPHANUMERICP or _); at a line edge,
;;;                                  to the previous end / next start
;;;   Home, Ctrl+A / End, Ctrl+E     line start / end
;;;   Up / Down                      previous / next line, same display
;;;                                  column or the nearest boundary left of
;;;                                  it; on the first / last row: history
;;;   Ctrl+K / Ctrl+U                kill to line end (at the end: join the
;;;                                  next line) / to line start
;;;   Ctrl+W                         delete the word before the cursor
;;;   Ctrl+G                         EDIT-EVENT with the value
;;;   Escape                         close the completion menu, else
;;;                                  CANCEL-EVENT (buffer kept)
;;;   Tab                            complete (below)
;;; Paste: one insert at the cursor; CRLF and CR become LF, Tab becomes four
;;; spaces, other control characters are dropped; never submits.
;;;
;;; History (oldest first): Up on the first row recalls the previous entry
;;; (saving the draft on the first step), Down on the last row the next one,
;;; and past the newest restores the draft. A recalled entry puts the cursor
;;; at its end. Any edit ends browsing. Submit appends the value unless it
;;; equals the newest entry, clears the buffer and ends browsing.
;;;
;;; Completion (for @ mentions): the TOKEN is the text of the cursor's line
;;; from after the last space or tab before the cursor up to the cursor.
;;; COMPLETION-FUNCTION takes the token and returns candidate strings (each
;;; replaces the whole token) or NIL.
;;;   - After typed text, when the token starts with one of
;;;     COMPLETION-TRIGGERS, the function is called and the menu opens on a
;;;     non-NIL result with the first candidate selected.
;;;   - While the menu is open, every insert or delete calls it again; NIL
;;;     closes the menu; the function is called at most once per edit.
;;;     Up / Down move the selection (wrapping). Tab or
;;;     Enter accept: the token becomes the candidate, the menu closes and
;;;     does not reopen until the next typed text. Escape closes it. Left,
;;;     Right, Home, End close it and move.
;;;   - Tab with the menu closed calls the function on the token: one
;;;     candidate is accepted at once, several open the menu, NIL does
;;;     nothing. Without a function, Tab does nothing.
;;;
;;; View on a W x H canvas, prompt width PW, text width TW = W - PW:
;;;   - The prompt sits at column 0 of the first row; every other row starts
;;;     at column PW. Each line wraps by cells at TW (whole clusters; a wide
;;;     character that does not fit moves to the next row). A cursor at the
;;;     end of a line that exactly fills its last row is drawn at column 0
;;;     of one extra row.
;;;   - The cursor cell gets the inverse attribute added (on a wide
;;;     character, both of its cells); unfocused, no cursor is drawn.
;;;   - An empty buffer with a placeholder shows it after the prompt, faint,
;;;     with the cursor on its first cell.
;;;   - With the menu open it takes K = min(candidates, COMPLETION-MAX-ITEMS,
;;;     H - 1) rows; the text gets R = H - K. The menu rows come right after
;;;     the last text row drawn, from column 0: "> " before the selected
;;;     candidate, "  " before the others (truncated to W), a window of K
;;;     candidates from top = clamp(selected - K + 1, 0, candidates - K).
;;;   - Without the menu R = H. When the text has more rows than R, the
;;;     window shows rows [top, top + R) with
;;;     top = clamp(cursor-row - R + 1, 0, rows - R).

(defclass text-input (widget)
  ((prompt :initarg :prompt :initform "> " :accessor text-input-prompt)
   (placeholder :initarg :placeholder :initform nil :accessor text-input-placeholder)
   (history :initarg :history :initform '() :accessor text-input-history
            :documentation "Submitted values, oldest first.")
   (completion-function :initarg :completion-function :initform nil
                        :accessor text-input-completion-function)
   (completion-triggers :initarg :completion-triggers :initform '("@")
                        :accessor text-input-completion-triggers)
   (completion-max-items :initarg :completion-max-items :initform 5
                         :accessor text-input-completion-max-items)
   (initial-value :initarg :value :initform "")
   ;; Buffer and cursor. ROW indexes LINES; COL is a character index into
   ;; that line on a cluster boundary.
   (lines :initform (list "") :type list)
   (row :initform 0 :type (integer 0))
   (col :initform 0 :type (integer 0))
   ;; History browsing: index into HISTORY, or NIL; DRAFT is the buffer
   ;; saved on the first step.
   (browse :initform nil)
   (draft :initform "")
   ;; Completion menu: candidates (NIL = closed) and the selected index.
   (candidates :initform '())
   (cindex :initform 0)
   ;; Per-message flags: the buffer was edited / the edit was typed text.
   (edited :initform nil)
   (typed :initform nil))
  (:documentation "Multi-line input with history, paste, wide characters and
a completion hook. :VALUE is the initial text (cursor at its end)."))

(defmethod initialize-instance :after ((input text-input) &key)
  (%load input (slot-value input 'initial-value)))

;;; ------------------------------------------------------------------
;;; Text helpers

(defun %split-lines (string)
  "STRING split at #\\Newline (\"\" → (\"\"))."
  (loop for start = 0 then (1+ end)
        for end = (position #\Newline string :start start)
        collect (subseq string start end)
        while end))

(defun %normalize-newlines (string)
  "CRLF and CR → LF."
  (with-output-to-string (out)
    (let ((n (length string)))
      (do ((i 0 (1+ i))) ((>= i n))
        (let ((ch (char string i)))
          (if (char= ch #\Return)
              (progn (write-char #\Newline out)
                     (when (and (< (1+ i) n) (char= (char string (1+ i)) #\Newline))
                       (incf i)))
              (write-char ch out)))))))

(defun %clean-text (string)
  "Text to insert: CRLF / CR → LF, Tab → four spaces, other control
characters dropped."
  (with-output-to-string (out)
    (loop for ch across (%normalize-newlines string)
          for code = (char-code ch)
          do (cond ((char= ch #\Newline) (write-char ch out))
                   ((char= ch #\Tab) (write-string "    " out))
                   ((or (< code 32) (<= 127 code 159)))
                   (t (write-char ch out))))))

(defun %word-char-p (char)
  (or (alphanumericp char) (char= char #\_)))

(defun %column-index (line column)
  "The largest cluster boundary in LINE whose prefix is at most COLUMN cells."
  (%fit-end line 0 (length line) column))

;;; ------------------------------------------------------------------
;;; Accessors

(defun text-input-value (input)
  "The buffer as one string, lines joined with #\\Newline."
  (format nil "~{~A~^~%~}" (slot-value input 'lines)))

(defun %load (input string)
  "Replace the buffer without counting as an edit (history recall, clear
after submit); the cursor goes to the end, the menu closes."
  (with-slots (lines row col) input
    (setf lines (%split-lines (%normalize-newlines string))
          row (1- (length lines))
          col (length (car (last lines)))))
  (%close-menu input))

(defun (setf text-input-value) (string input)
  "Replace the buffer (CRLF / CR become LF); the cursor goes to the end,
browsing and completion end."
  (check-type string string)
  (%load input string)
  (setf (slot-value input 'browse) nil)
  string)

(defun text-input-lines (input)
  "The buffer's lines (at least one, possibly \"\")."
  (copy-list (slot-value input 'lines)))

(defun text-input-cursor (input)
  "→ (values row col): line index and character index in that line."
  (values (slot-value input 'row) (slot-value input 'col)))

(defun %line (input &optional (row (slot-value input 'row)))
  (nth row (slot-value input 'lines)))

(defun %last-row (input)
  (1- (length (slot-value input 'lines))))

(defun text-input-cursor-column (input)
  "The cursor's display column in its line, before wrapping (wide = 2,
combining = 0)."
  (tp:string-width (%line input) :end (slot-value input 'col)))

(defun text-input-completing-p (input)
  "True while the completion menu is open."
  (and (slot-value input 'candidates) t))

(defun text-input-completion-candidates (input)
  "The open menu's candidates, NIL when closed."
  (slot-value input 'candidates))

(defun text-input-completion-index (input)
  "Index of the selected candidate, NIL when closed."
  (and (slot-value input 'candidates) (slot-value input 'cindex)))

;;; ------------------------------------------------------------------
;;; Editing. %EDIT is the only writer of the buffer as an edit: it ends
;;; history browsing and flags the message as edited, and %AFTER-EDIT then
;;; runs the completion query at most once per message.

(defun %edit (input r0 c0 r1 c1 new)
  "Replace the text from (R0, C0) to (R1, C1) with NEW (which may hold
newlines); the cursor goes after NEW."
  (with-slots (lines row col browse edited) input
    (let* ((before (subseq (nth r0 lines) 0 c0))
           (after (subseq (nth r1 lines) c1))
           (pieces (%split-lines new))
           (n (length pieces))
           (last-piece (car (last pieces)))
           (new-lines (if (= n 1)
                          (list (concatenate 'string before last-piece after))
                          (append (list (concatenate 'string before (first pieces)))
                                  (subseq pieces 1 (1- n))
                                  (list (concatenate 'string last-piece after))))))
      (setf lines (append (subseq lines 0 r0) new-lines (nthcdr (1+ r1) lines))
            row (+ r0 n -1)
            col (if (= n 1) (+ c0 (length last-piece)) (length last-piece))
            browse nil
            edited t))))

(defun %insert (input string)
  (when (plusp (length string))
    (multiple-value-bind (r c) (text-input-cursor input)
      (%edit input r c r c string))))

(defun %after-edit (input)
  "Completion after a message: one re-query while the menu is open, or one
query after typed text whose token starts with a trigger."
  (with-slots (edited typed candidates completion-function) input
    (when (and edited completion-function)
      (cond (candidates (%query input))
            ((and typed (%triggered-p input)) (%query input))))
    (setf edited nil typed nil)))

(defun text-input-insert (input string)
  "Insert STRING at the cursor as a paste would. → INPUT."
  (check-type string string)
  (setf (slot-value input 'edited) nil (slot-value input 'typed) nil)
  (%insert input (%clean-text string))
  (%after-edit input)
  input)

;;; Positions

(defun %prev-position (input)
  "(values row col) one cluster before the cursor, or NIL at the start."
  (with-slots (row col) input
    (cond ((> col 0) (values row (%cluster-start (%line input) col)))
          ((> row 0) (values (1- row) (length (%line input (1- row)))))
          (t nil))))

(defun %next-position (input)
  "(values row col) one cluster after the cursor, or NIL at the end."
  (with-slots (row col) input
    (let ((line (%line input)))
      (cond ((< col (length line)) (values row (%cluster-end line col)))
            ((< row (%last-row input)) (values (1+ row) 0))
            (t nil)))))

(defun %word-left-position (input)
  (with-slots (row col) input
    (if (zerop col)
        (if (> row 0)
            (values (1- row) (length (%line input (1- row))))
            (values row col))
        (let ((line (%line input)) (i col))
          (loop while (and (> i 0)
                           (not (%word-char-p (char line (%cluster-start line i)))))
                do (setf i (%cluster-start line i)))
          (loop while (and (> i 0)
                           (%word-char-p (char line (%cluster-start line i))))
                do (setf i (%cluster-start line i)))
          (values row i)))))

(defun %word-right-position (input)
  (with-slots (row col) input
    (let ((line (%line input)))
      (if (= col (length line))
          (if (< row (%last-row input))
              (values (1+ row) 0)
              (values row col))
          (let ((i col) (n (length line)))
            (loop while (and (< i n) (not (%word-char-p (char line i))))
                  do (setf i (%cluster-end line i)))
            (loop while (and (< i n) (%word-char-p (char line i)))
                  do (setf i (%cluster-end line i)))
            (values row i))))))

(defun %move-to (input r c)
  (when r
    (setf (slot-value input 'row) r (slot-value input 'col) c)))

(defun %delete-to (input r &optional c)
  "Delete between the cursor and (R, C), either side."
  (when r
    (with-slots (row col) input
      (if (or (< r row) (and (= r row) (< c col)))
          (%edit input r c row col "")
          (unless (and (= r row) (= c col))
            (%edit input row col r c ""))))))

;;; History

(defun %history-step (input direction)
  "DIRECTION -1 recalls an older entry, +1 a newer one (past the newest: the
draft). → true when it acted."
  (with-slots (history browse draft) input
    (let ((n (length history)))
      (cond ((minusp direction)
             (when (plusp n)
               (if browse
                   (setf browse (max 0 (1- browse)))
                   (setf draft (text-input-value input) browse (1- n)))
               (%load input (nth browse history))
               t))
            (browse
             (incf browse)
             (if (>= browse n)
                 (progn (setf browse nil) (%load input draft))
                 (%load input (nth browse history)))
             t)))))

(defun %vertical (input delta)
  (with-slots (row) input
    (let ((target (+ row delta)))
      (if (<= 0 target (%last-row input))
          (let ((column (text-input-cursor-column input)))
            (%close-menu input)
            (%move-to input target (%column-index (%line input target) column)))
          (%history-step input delta)))))

(defun %submit (input)
  (let ((value (text-input-value input)))
    (with-slots (history browse) input
      (unless (equal value (car (last history)))
        (setf history (append history (list value))))
      (%load input "")
      (setf browse nil))
    (make-submit-event :widget input :text value)))

;;; Completion

(defun %token-start (input)
  (let ((line (%line input)) (col (slot-value input 'col)))
    (1+ (or (position-if (lambda (ch) (member ch '(#\Space #\Tab))) line
                         :end col :from-end t)
            -1))))

(defun %token (input)
  (subseq (%line input) (%token-start input) (slot-value input 'col)))

(defun %triggered-p (input)
  (let ((token (%token input)))
    (some (lambda (trigger)
            (and (<= (length trigger) (length token))
                 (string= trigger token :end2 (length trigger))))
          (text-input-completion-triggers input))))

(defun %close-menu (input)
  (setf (slot-value input 'candidates) '() (slot-value input 'cindex) 0))

(defun %query (input)
  "Call the completion function on the token (its errors propagate); open
the menu on a non-NIL result, else close it."
  (let ((found (funcall (text-input-completion-function input) (%token input))))
    (if found
        (setf (slot-value input 'candidates) (copy-list found)
              (slot-value input 'cindex) 0)
        (%close-menu input))
    found))

(defun %replace-token (input candidate)
  (%close-menu input)
  (with-slots (row col) input
    (%edit input row (%token-start input) row col candidate)))

(defun %accept (input)
  (with-slots (candidates cindex) input
    (%replace-token input (nth cindex candidates))))

(defun %tab-complete (input)
  (when (text-input-completion-function input)
    (let ((found (%query input)))
      (when (and found (null (rest found)))
        (%replace-token input (first found))))))

(defun %menu-step (input delta)
  (with-slots (candidates cindex) input
    (setf cindex (mod (+ cindex delta) (length candidates)))))

;;; ------------------------------------------------------------------
;;; Keys

(defun %key (input message)
  "Act on a live key MESSAGE. → an event or NIL."
  (let* ((key (tui:key-message-key message))
         (mods (tui:key-message-modifiers message))
         (ctrl (member :ctrl mods))
         (alt (member :alt mods))
         (plain (null (intersection mods '(:ctrl :alt :super))))
         (bare (null (intersection mods '(:ctrl :alt :shift :super))))
         (menu (slot-value input 'candidates)))
    (flet ((move (function)
             (%close-menu input)
             (multiple-value-bind (r c) (funcall function input)
               (%move-to input r c))
             nil))
      (cond
        ((eq key :enter)
         (with-slots (row col) input
           (cond ((not bare) (%insert input (string #\Newline)) nil)
                 (menu (%accept input) nil)
                 ((and (> col 0) (char= #\\ (char (%line input) (1- col))))
                  (%edit input row (1- col) row col (string #\Newline))
                  nil)
                 ((string= "" (text-input-value input)) nil)
                 (t (%submit input)))))
        ((and ctrl (eq key :j)) (%insert input (string #\Newline)) nil)
        ((eq key :tab) (if menu (%accept input) (%tab-complete input)) nil)
        ((eq key :escape)
         (if menu
             (progn (%close-menu input) nil)
             (make-cancel-event :widget input)))
        ((and menu (eq key :arrow-up)) (%menu-step input -1) nil)
        ((and menu (eq key :arrow-down)) (%menu-step input 1) nil)
        ((or (and (eq key :arrow-left) (or ctrl alt)) (and alt (eq key :b)))
         (move #'%word-left-position))
        ((or (and (eq key :arrow-right) (or ctrl alt)) (and alt (eq key :f)))
         (move #'%word-right-position))
        ((eq key :arrow-left) (move #'%prev-position))
        ((eq key :arrow-right) (move #'%next-position))
        ((or (eq key :home) (and ctrl (eq key :a)))
         (move (lambda (in) (values (slot-value in 'row) 0))))
        ((or (eq key :end) (and ctrl (eq key :e)))
         (move (lambda (in) (values (slot-value in 'row) (length (%line in))))))
        ((eq key :arrow-up) (%vertical input -1) nil)
        ((eq key :arrow-down) (%vertical input 1) nil)
        ((or (eq key :backspace) (and ctrl (eq key :h)))
         (multiple-value-call #'%delete-to input (%prev-position input)) nil)
        ((eq key :delete)
         (multiple-value-call #'%delete-to input (%next-position input)) nil)
        ((and ctrl (eq key :k))
         (with-slots (row col) input
           (if (< col (length (%line input)))
               (%delete-to input row (length (%line input)))
               (multiple-value-call #'%delete-to input (%next-position input))))
         nil)
        ((and ctrl (eq key :u)) (%delete-to input (slot-value input 'row) 0) nil)
        ((and ctrl (eq key :w))
         (multiple-value-call #'%delete-to input (%word-left-position input)) nil)
        ((and ctrl (eq key :g)) (make-edit-event :widget input :text (text-input-value input)))
        ((and plain (tui:key-message-text message))
         (let ((text (%clean-text (tui:key-message-text message))))
           (when (plusp (length text))
             (setf (slot-value input 'typed) t)
             (%insert input text)))
         nil)
        (t nil)))))

(defmethod widget-update ((input text-input) message)
  (setf (slot-value input 'edited) nil (slot-value input 'typed) nil)
  (let ((event (typecase message
                 (tui:paste-message
                  (%insert input (%clean-text (tui:paste-message-text message)))
                  nil)
                 (tui:key-message (%key input message))
                 (t nil))))
    (%after-edit input)
    (values input nil event)))

;;; ------------------------------------------------------------------
;;; View

(defun %invert-cell (canvas x y)
  "Add the inverse attribute to the cell at X, Y (and to a wide cell's spacer)."
  (when (and (< -1 x (tui:canvas-cols canvas)) (< -1 y (tui:canvas-rows canvas)))
    (let* ((old (tui:canvas-cell canvas x y))
           (style (tp:copy-style (tp:cell-style old))))
      (setf (tp:style-inverse style) t)
      (setf (tui:canvas-cell canvas x y)
            (tp:make-cell :text (tp:cell-text old) :width (tp:cell-width old) :style style))
      (when (eq :wide (tp:cell-width old))
        (%invert-cell canvas (1+ x) y)))))

(defun %clamp (value low high)
  (max low (min value high)))

(defun %faint-runs (runs)
  (mapcar (lambda (run)
            (%make-text-run (text-run-text run)
                            (let ((s (tp:copy-style (text-run-style run))))
                              (setf (tp:style-faint s) t)
                              s)))
          (as-runs runs)))

(defmethod widget-view ((input text-input) canvas)
  (let ((w (tui:canvas-cols canvas))
        (h (tui:canvas-rows canvas)))
    (setf (widget-width input) w (widget-height input) h)
    (when (or (zerop w) (zerop h))
      (return-from widget-view nil))
    (let* ((prompt (text-input-prompt input))
           (pw (tp:string-width prompt))
           (tw (max 1 (- w pw)))
           (rows '())                   ; (line start end), reversed while built
           (count 0)
           (cursor-row 0)
           (cursor-x pw))
      (multiple-value-bind (crow ccol) (text-input-cursor input)
        (loop for line in (slot-value input 'lines)
              for li from 0
              do (let ((ranges (%break-ranges line 0 (length line) tw :words nil)))
                   (loop for ((start . end) . more) on ranges
                         do (push (list line start end) rows)
                            (when (and (= li crow)
                                       (or (< ccol end) (and (null more) (= ccol end)))
                                       (<= start ccol))
                              (if (and (null more) (= ccol end) (< start end)
                                       (= tw (tp:string-width line :start start :end end)))
                                  ;; A full last row: the cursor goes on an extra row.
                                  (progn (incf count)
                                         (push (list line end end) rows)
                                         (setf cursor-row count cursor-x pw))
                                  (setf cursor-row count
                                        cursor-x (+ pw (tp:string-width line :start start
                                                                             :end ccol)))))
                            (incf count)))))
      (setf rows (coerce (nreverse rows) 'vector))
      (let* ((n (length rows))
             (candidates (slot-value input 'candidates))
             (k (if candidates
                    (max 0 (min (length candidates) (text-input-completion-max-items input)
                                (1- h)))
                    0))
             (r (- h k))
             (top (%clamp (- cursor-row r -1) 0 (max 0 (- n r))))
             (shown (min r (- n top))))
        (when (zerop top)
          (tui:draw-text canvas 0 0 prompt))
        (loop for i from top below (+ top shown)
              for (line start end) = (aref rows i)
              do (tui:draw-text canvas pw (- i top) (subseq line start end)))
        (let ((placeholder (text-input-placeholder input)))
          (when (and placeholder (zerop top)
                     (equal '("") (slot-value input 'lines)))
            (draw-runs canvas pw 0 (%faint-runs placeholder))))
        (when (and (widget-focused-p input) (<= top cursor-row) (< cursor-row (+ top r)))
          (%invert-cell canvas cursor-x (- cursor-row top)))
        (when (plusp k)
          (let* ((index (slot-value input 'cindex))
                 (mtop (%clamp (- index k -1) 0 (- (length candidates) k))))
            (loop for j below k
                  for ci = (+ mtop j)
                  do (draw-runs canvas 0 (+ shown j)
                                (truncate-runs (concatenate 'string
                                                            (if (= ci index) "> " "  ")
                                                            (nth ci candidates))
                                               w)))))))))
