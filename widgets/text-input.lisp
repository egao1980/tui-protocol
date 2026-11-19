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
   (initial-value :initarg :value :initform ""))
  (:documentation "Multi-line input with history, paste, wide characters and
a completion hook. :VALUE is the initial text (cursor at its end)."))

(defun text-input-value (input)
  "The buffer as one string, lines joined with #\\Newline."
  (declare (ignore input))
  (%todo 'text-input-value))

(defun (setf text-input-value) (string input)
  "Replace the buffer (CRLF / CR become LF); the cursor goes to the end,
browsing and completion end."
  (declare (ignore string input))
  (%todo '(setf text-input-value)))

(defun text-input-lines (input)
  "The buffer's lines (at least one, possibly \"\")."
  (declare (ignore input))
  (%todo 'text-input-lines))

(defun text-input-cursor (input)
  "→ (values row col): line index and character index in that line."
  (declare (ignore input))
  (%todo 'text-input-cursor))

(defun text-input-cursor-column (input)
  "The cursor's display column in its line, before wrapping (wide = 2,
combining = 0)."
  (declare (ignore input))
  (%todo 'text-input-cursor-column))

(defun text-input-insert (input string)
  "Insert STRING at the cursor as a paste would. → INPUT."
  (declare (ignore input string))
  (%todo 'text-input-insert))

(defun text-input-completing-p (input)
  "True while the completion menu is open."
  (declare (ignore input))
  (%todo 'text-input-completing-p))

(defun text-input-completion-candidates (input)
  "The open menu's candidates, NIL when closed."
  (declare (ignore input))
  (%todo 'text-input-completion-candidates))

(defun text-input-completion-index (input)
  "Index of the selected candidate, NIL when closed."
  (declare (ignore input))
  (%todo 'text-input-completion-index))

(defmethod widget-update ((input text-input) message)
  (declare (ignore message))
  (%todo 'widget-update))

(defmethod widget-view ((input text-input) canvas)
  (declare (ignore canvas))
  (%todo 'widget-view))
