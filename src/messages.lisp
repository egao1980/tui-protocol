(in-package #:tui-protocol)

;;; Messages a program's TUI-UPDATE receives. Key names, modifiers and mouse
;;; buttons are terminal-protocol's (+KEY-NAMES+, +MODIFIERS+, +MOUSE-BUTTONS+).

(defstruct message)

(defstruct (key-message (:include message))
  (key :unidentified :type keyword)
  (text nil :type (or null string))
  (modifiers '() :type list)
  (action :press :type (member :press :repeat :release)))

(defstruct (mouse-message (:include message))
  (button nil :type (or null keyword))
  (action :press :type (member :press :release :motion))
  (x 0 :type (integer 0))
  (y 0 :type (integer 0))
  (modifiers '() :type list))

(defstruct (paste-message (:include message))
  (text "" :type string))

(defstruct (focus-message (:include message))
  (focused-p t :type boolean))

(defstruct (resize-message (:include message))
  (cols 80 :type (integer 1))
  (rows 24 :type (integer 1)))

(defstruct (tick-message (:include message))
  (time 0 :type real))
