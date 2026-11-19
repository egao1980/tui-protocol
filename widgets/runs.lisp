(in-package #:tui-protocol/widgets)

;;; H1b contract skeleton: every function body below signals until the
;;; milestone is implemented. The docstrings are the contract; the brief is
;;; cl-stack docs/capabilities/tui.md, section "H1b — widgets, locked".

(defun %todo (name)
  (error "~S is not implemented yet (tui-protocol/widgets H1b contract)" name))

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
  (declare (ignore text style bold dim italic underline inverse foreground background))
  (%todo 'text-run))

(defun as-runs (line)
  "LINE as a list of runs: a string → one default-style run (\"\" → ()), a
TEXT-RUN → a list of it, a list of runs → itself."
  (declare (ignore line))
  (%todo 'as-runs))

(defun runs-string (runs)
  "The concatenated text of RUNS (anything AS-RUNS takes)."
  (declare (ignore runs))
  (%todo 'runs-string))

(defun runs-width (runs)
  "Display width of RUNS in cells (TP:STRING-WIDTH: wide = 2, combining = 0)."
  (declare (ignore runs))
  (%todo 'runs-width))

(defun draw-runs (canvas x y runs)
  "Draw RUNS on one row of CANVAS from X, each in its style, clipped like
TUI:DRAW-TEXT. → the column after the text."
  (declare (ignore canvas x y runs))
  (%todo 'draw-runs))

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
  (declare (ignore runs width))
  (%todo 'wrap-runs))

(defun truncate-runs (runs width &key (ellipsis "…"))
  "RUNS cut to at most WIDTH cells. When cut, the result ends with ELLIPSIS
(in the style of the last kept run) and the whole is still ≤ WIDTH."
  (declare (ignore runs width ellipsis))
  (%todo 'truncate-runs))
