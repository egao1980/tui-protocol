(in-package #:tui-protocol/tests)

;;; Round trips: whatever the emulator's ENCODE-* produce, the decoder must
;;; turn back into the same event.

(defun decode-all (bytes &key split)
  (let ((d (tui:make-input-decoder)))
    (append (if split
                (loop for b across bytes
                      append (tui:decode-input d (make-array 1 :element-type '(unsigned-byte 8)
                                                               :initial-element b)))
                (tui:decode-input d bytes))
            (tui:flush-input d))))

(defun key-round-trip (term key &rest args &key text modifiers &allow-other-keys)
  (let* ((bytes (apply #'tp:encode-key term key args))
         (messages (decode-all bytes))
         (split (decode-all bytes :split t))
         (m (first messages)))
    (and (= 1 (length messages))
         (equalp messages split)
         (tui:key-message-p m)
         (eq (tui:key-message-key m) key)
         (equal (sort (copy-list (tui:key-message-modifiers m)) #'string<)
                (sort (copy-list modifiers) #'string<))
         ;; With Ctrl/Alt the terminal sends no text, so there is none to compare.
         (or (null text) (intersection modifiers '(:ctrl :alt))
             (equal (tui:key-message-text m) text)))))

(defparameter *keys*
  '((:a :text "a") (:z :text "z") (:a :text "A" :modifiers (:shift))
    (:digit-1 :text "1") (:digit-1 :text "!" :modifiers (:shift)) (:space :text " ")
    (:minus :text "-") (:slash :text "?" :modifiers (:shift))
    (:enter) (:tab) (:backspace) (:escape)
    (:arrow-up) (:arrow-down) (:arrow-left) (:arrow-right) (:home) (:end)
    (:page-up) (:page-down) (:insert) (:delete)
    (:f1) (:f2) (:f3) (:f4) (:f5) (:f6) (:f7) (:f8) (:f9) (:f10) (:f11) (:f12)
    (:c :text "c" :modifiers (:ctrl)) (:x :text "x" :modifiers (:alt))
    (:arrow-up :modifiers (:ctrl)) (:arrow-right :modifiers (:shift))
    (:arrow-left :modifiers (:alt :shift)) (:f5 :modifiers (:ctrl))
    (:delete :modifiers (:alt)) (:home :modifiers (:ctrl)) (:tab :modifiers (:shift))))

(deftest keys-round-trip
  (with-emulator (term 10 2)
    (dolist (k *keys*)
      (ok (apply #'key-round-trip term k) (format nil "~S" k)))))

(deftest keys-round-trip-in-application-cursor-mode
  (with-emulator (term 10 2)
    (tp:terminal-feed term (octets (format nil "~C[?1h" #\Esc)))
    (dolist (k '((:arrow-up) (:arrow-left) (:home) (:end)))
      (ok (apply #'key-round-trip term k) (format nil "~S" k)))))

(deftest non-ascii-text
  (let ((m (first (decode-all (octets "é")))))
    (ok (equal (tui:key-message-text m) "é"))
    (ok (eq (tui:key-message-key m) :unidentified))))

(deftest mouse-round-trip
  (with-emulator (term 20 10)
    (dolist (format '("1006h" nil))
      (tp:terminal-feed term (octets (format nil "~C[?1002h" #\Esc)))
      (when format (tp:terminal-feed term (octets (format nil "~C[?~A" #\Esc format))))
      (dolist (event '((:left :press 3 4 ()) (:left :release 3 4 ()) (:right :press 0 0 (:shift))
                       (:wheel-up :press 5 5 ()) (:left :motion 6 2 ()) (:middle :press 1 1 (:ctrl))))
        (destructuring-bind (button action x y mods) event
          (let ((m (first (decode-all (tp:encode-mouse term button action x y :modifiers mods)))))
            (ok (and (tui:mouse-message-p m)
                     (= x (tui:mouse-message-x m)) (= y (tui:mouse-message-y m))
                     (eq action (tui:mouse-message-action m))
                     ;; X10 releases carry no button.
                     (or (and (null format) (eq action :release))
                         (eq button (tui:mouse-message-button m)))
                     (equal mods (tui:mouse-message-modifiers m)))
                (format nil "~A ~S" (or format "x10") event)))))
      (tp:terminal-feed term (octets (format nil "~C[?1006l" #\Esc))))))

(deftest paste-and-focus
  (with-emulator (term 10 2)
    (tp:terminal-feed term (octets (format nil "~C[?2004h~C[?1004h" #\Esc #\Esc)))
    (let ((ms (decode-all (concatenate '(vector (unsigned-byte 8))
                                       (tp:encode-paste term "a b") (tp:encode-focus term nil)
                                       (tp:encode-key term :a :text "a")))))
      (ok (= 3 (length ms)))
      (ok (equal (tui:paste-message-text (first ms)) "a b"))
      (ok (not (tui:focus-message-focused-p (second ms))))
      (ok (eq :a (tui:key-message-key (third ms)))))
    ;; A paste split byte by byte still arrives whole.
    (let ((ms (decode-all (tp:encode-paste term "split me") :split t)))
      (ok (equal (mapcar #'tui:paste-message-text ms) '("split me"))))))

(deftest lone-escape-needs-flush
  (let ((d (tui:make-input-decoder)))
    (ok (null (tui:decode-input d (octets (string #\Esc)))))
    (ok (eq :escape (tui:key-message-key (first (tui:flush-input d)))))))
