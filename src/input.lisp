(in-package #:tui-protocol)

;;; Bytes a terminal sends → messages. The inverse of terminal-protocol's
;;; ENCODE-KEY / ENCODE-MOUSE / ENCODE-PASTE / ENCODE-FOCUS: tests round-trip
;;; every encoding through this decoder.

(defstruct (input-decoder (:constructor make-input-decoder))
  (buffer (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
  (paste nil))   ; octet vector while inside a bracketed paste

;;; Printable characters → (key . shifted-p) on a US layout.

(defparameter +char-keys+
  (let ((table (make-hash-table)))
    (loop for c across "abcdefghijklmnopqrstuvwxyz"
          do (setf (gethash c table) (cons (intern (string-upcase (string c)) :keyword) nil)
                   (gethash (char-upcase c) table)
                   (cons (intern (string-upcase (string c)) :keyword) t)))
    (loop for d from 0 to 9
          for shifted across ")!@#$%^&*("
          for key = (intern (format nil "DIGIT-~D" d) :keyword)
          do (setf (gethash (code-char (+ 48 d)) table) (cons key nil)
                   (gethash shifted table) (cons key t)))
    (loop for (plain shifted key) in '((#\` #\~ :backquote) (#\- #\_ :minus) (#\= #\+ :equal)
                                       (#\[ #\{ :bracket-left) (#\] #\} :bracket-right)
                                       (#\\ #\| :backslash) (#\; #\: :semicolon)
                                       (#\' #\" :quote) (#\, #\< :comma) (#\. #\> :period)
                                       (#\/ #\? :slash))
          do (setf (gethash plain table) (cons key nil)
                   (gethash shifted table) (cons key t)))
    (setf (gethash #\Space table) (cons :space nil))
    table))

(defun %char-key (char modifiers)
  (let ((entry (gethash char +char-keys+)))
    (if entry
        (make-key-message :key (car entry) :text (string char)
                          :modifiers (if (cdr entry) (adjoin :shift modifiers) modifiers))
        (make-key-message :key :unidentified :text (string char) :modifiers modifiers))))

(defun %control-key (byte)
  "C0 byte → key message (Ctrl+letter for the ones without a key of their own)."
  (case byte
    (13 (make-key-message :key :enter))
    (9 (make-key-message :key :tab))
    (127 (make-key-message :key :backspace))
    (8 (make-key-message :key :h :modifiers '(:ctrl)))
    (0 (make-key-message :key :space :modifiers '(:ctrl)))
    (28 (make-key-message :key :backslash :modifiers '(:ctrl)))
    (29 (make-key-message :key :bracket-right :modifiers '(:ctrl)))
    (30 (make-key-message :key :digit-6 :modifiers '(:ctrl)))
    (31 (make-key-message :key :minus :modifiers '(:ctrl)))
    (t (make-key-message :key (intern (string (code-char (+ 64 byte))) :keyword)
                         :modifiers '(:ctrl)))))

(defun %modifiers (param)
  "xterm modifier parameter (1 + bits) → modifier keywords."
  (if (or (null param) (< param 2))
      '()
      (let ((bits (1- param)) (out '()))
        (when (logbitp 0 bits) (push :shift out))
        (when (logbitp 1 bits) (push :alt out))
        (when (logbitp 2 bits) (push :ctrl out))
        (when (logbitp 3 bits) (push :super out))
        (nreverse out))))

(defparameter +tilde-keys+
  '((1 . :home) (2 . :insert) (3 . :delete) (4 . :end) (5 . :page-up) (6 . :page-down)
    (7 . :home) (8 . :end) (11 . :f1) (12 . :f2) (13 . :f3) (14 . :f4) (15 . :f5)
    (17 . :f6) (18 . :f7) (19 . :f8) (20 . :f9) (21 . :f10) (23 . :f11) (24 . :f12)))

(defparameter +letter-finals+
  '((#\A . :arrow-up) (#\B . :arrow-down) (#\C . :arrow-right) (#\D . :arrow-left)
    (#\H . :home) (#\F . :end) (#\P . :f1) (#\Q . :f2) (#\R . :f3) (#\S . :f4)))

(defun %codepoint-key (code modifiers)
  "Unicode code point (CSI-u, modifyOtherKeys) → key message."
  (case code
    (13 (make-key-message :key :enter :modifiers modifiers))
    (9 (make-key-message :key :tab :modifiers modifiers))
    (27 (make-key-message :key :escape :modifiers modifiers))
    (127 (make-key-message :key :backspace :modifiers modifiers))
    (t (let ((m (%char-key (code-char code) modifiers)))
         ;; With Ctrl/Alt the text is not inserted.
         (when (intersection modifiers '(:ctrl :alt :super))
           (setf (key-message-text m) nil))
         m))))

(defun %mouse-button (b)
  (let ((low (logand b 3)))
    (cond ((logbitp 6 b) (nth low '(:wheel-up :wheel-down :wheel-left :wheel-right)))
          ((logbitp 7 b) (nth low '(:back :forward :button-10 :button-11)))
          (t (nth low '(:left :middle :right nil))))))

(defun %mouse-modifiers (b)
  (let ((out '()))
    (when (logbitp 2 b) (push :shift out))
    (when (logbitp 3 b) (push :alt out))
    (when (logbitp 4 b) (push :ctrl out))
    (nreverse out)))

(defun %mouse (b x y release-p)
  (let ((motion (logbitp 5 b))
        (button (%mouse-button b)))
    (make-mouse-message :button button
                        :action (cond (motion :motion)
                                      ((or release-p (and (null button) (not motion))) :release)
                                      (t :press))
                        :x (max 0 x) :y (max 0 y)
                        :modifiers (%mouse-modifiers b))))

(defun %parse-params (bytes start end)
  "\"1;5\" → (1 5); empty fields → NIL. Sub-parameters (colon) keep the first."
  (let ((params '()) (current nil) (skip nil))
    (loop for i from start below end
          for c = (code-char (aref bytes i))
          do (cond ((char= c #\;) (push current params) (setf current nil skip nil))
                   ((char= c #\:) (setf skip t))
                   ((digit-char-p c)
                    (unless skip (setf current (+ (* 10 (or current 0)) (digit-char-p c)))))))
    (push current params)
    (nreverse params)))

(defun %utf8-length (lead)
  (cond ((< lead #x80) 1) ((< lead #xc0) 1) ((< lead #xe0) 2) ((< lead #xf0) 3) (t 4)))

(defun %utf8-char (bytes i n)
  (let ((lead (aref bytes i)))
    (code-char
     (case n
       (1 lead)
       (2 (logior (ash (logand lead #x1f) 6) (logand (aref bytes (+ i 1)) #x3f)))
       (3 (logior (ash (logand lead #x0f) 12) (ash (logand (aref bytes (+ i 1)) #x3f) 6)
                  (logand (aref bytes (+ i 2)) #x3f)))
       (t (logior (ash (logand lead #x07) 18) (ash (logand (aref bytes (+ i 1)) #x3f) 12)
                  (ash (logand (aref bytes (+ i 2)) #x3f) 6) (logand (aref bytes (+ i 3)) #x3f)))))))

(defun %utf8-string (bytes)
  (with-output-to-string (s)
    (let ((i 0) (n (length bytes)))
      (loop while (< i n)
            do (let ((len (min (%utf8-length (aref bytes i)) (- n i))))
                 (write-char (%utf8-char bytes i len) s)
                 (incf i len))))))

(defun %csi (bytes start end final)
  "Decode CSI with parameter bytes BYTES[START, END) and FINAL. → message or NIL."
  (let* ((private (and (< start end) (char= (code-char (aref bytes start)) #\<)))
         (params (%parse-params bytes (if private (1+ start) start) end))
         (p1 (first params))
         (mods (%modifiers (second params))))
    (cond (private
           (when (member final '(#\M #\m))
             (destructuring-bind (&optional (b 0) (x 1) (y 1)) params
               (%mouse (or b 0) (1- (or x 1)) (1- (or y 1)) (char= final #\m)))))
          ((char= final #\~)
           (cond ((eql p1 27)
                  (%codepoint-key (or (third params) 0) (%modifiers (second params))))
                 (t (let ((key (cdr (assoc p1 +tilde-keys+))))
                      (when key (make-key-message :key key :modifiers mods))))))
          ((char= final #\u)
           (%codepoint-key (or p1 0) mods))
          ((char= final #\Z) (make-key-message :key :tab :modifiers (adjoin :shift mods)))
          ((char= final #\I) (make-focus-message :focused-p t))
          ((char= final #\O) (make-focus-message :focused-p nil))
          (t (let ((key (cdr (assoc final +letter-finals+))))
               (when key (make-key-message :key key :modifiers mods)))))))

(defun %decode-one (bytes i n)
  "Decode the sequence starting at I. → (values message-or-nil next-index), or
(values :incomplete i) when more bytes are needed."
  (let ((b (aref bytes i)))
    (cond
      ;; ESC-prefixed
      ((= b 27)
       (when (>= (1+ i) n) (return-from %decode-one (values :incomplete i)))
       (let ((c (aref bytes (1+ i))))
         (cond
           ;; CSI
           ((= c 91)
            (let ((j (+ i 2)))
              (when (>= j n) (return-from %decode-one (values :incomplete i)))
              ;; X10 mouse: ESC [ M b x y
              (when (= (aref bytes j) 77)
                (when (> (+ j 4) n) (return-from %decode-one (values :incomplete i)))
                (let ((cb (- (aref bytes (+ j 1)) 32)))
                  (return-from %decode-one
                    (values (%mouse cb (- (aref bytes (+ j 2)) 33) (- (aref bytes (+ j 3)) 33)
                                    (= (logand cb 3) 3))
                            (+ j 4)))))
              (loop while (and (< j n) (<= #x20 (aref bytes j) #x3f)) do (incf j))
              (when (>= j n) (return-from %decode-one (values :incomplete i)))
              (let ((final (code-char (aref bytes j))))
                (values (%csi bytes (+ i 2) j final) (1+ j)))))
           ;; SS3
           ((= c 79)
            (when (>= (+ i 2) n) (return-from %decode-one (values :incomplete i)))
            (let ((key (cdr (assoc (code-char (aref bytes (+ i 2))) +letter-finals+))))
              (values (and key (make-key-message :key key)) (+ i 3))))
           ;; Alt + key
           (t (multiple-value-bind (m next) (%decode-one bytes (1+ i) n)
                (cond ((eq m :incomplete) (values :incomplete i))
                      ((key-message-p m)
                       (setf (key-message-modifiers m) (adjoin :alt (key-message-modifiers m)))
                       (when (string= (or (key-message-text m) "") (string (code-char 27)))
                         (setf (key-message-text m) nil))
                       (values m next))
                      (t (values m next))))))))
      ((or (< b 32) (= b 127)) (values (%control-key b) (1+ i)))
      (t (let ((len (%utf8-length b)))
           (if (> (+ i len) n)
               (values :incomplete i)
               (values (%char-key (%utf8-char bytes i len) '()) (+ i len))))))))

(defparameter +paste-end+ #(27 91 50 48 49 126))   ; ESC [ 2 0 1 ~
(defparameter +paste-start+ #(27 91 50 48 48 126)) ; ESC [ 2 0 0 ~

(defun decode-input (decoder octets)
  "Feed OCTETS to DECODER. → the complete messages, in order. A partial
sequence stays buffered for the next call; call FLUSH-INPUT after a short
timeout to release a lone ESC as the Escape key."
  (let ((buffer (input-decoder-buffer decoder)) (out '()))
    (loop for b across octets do (vector-push-extend b buffer))
    (let ((i 0) (n (length buffer)))
      (loop while (< i n)
            do (if (input-decoder-paste decoder)
                   (let ((endp (search +paste-end+ buffer :start2 i)))
                     (if endp
                         (let ((paste (input-decoder-paste decoder)))
                           (loop for k from i below endp do (vector-push-extend (aref buffer k) paste))
                           (push (make-paste-message :text (%utf8-string paste)) out)
                           (setf (input-decoder-paste decoder) nil
                                 i (+ endp (length +paste-end+))))
                         ;; Keep a possible partial terminator in the buffer.
                         (let ((keep (max i (- n (1- (length +paste-end+))))))
                           (loop for k from i below keep
                                 do (vector-push-extend (aref buffer k) (input-decoder-paste decoder)))
                           (setf i keep)
                           (return))))
                   (if (and (<= (+ i (length +paste-start+)) n)
                            (not (mismatch +paste-start+ buffer :start2 i
                                                                :end2 (+ i (length +paste-start+)))))
                       (setf (input-decoder-paste decoder)
                             (make-array 0 :element-type '(unsigned-byte 8)
                                           :adjustable t :fill-pointer 0)
                             i (+ i (length +paste-start+)))
                       (multiple-value-bind (m next) (%decode-one buffer i n)
                         (when (eq m :incomplete) (return))
                         (when m (push m out))
                         (setf i next)))))
      ;; Drop what was consumed.
      (let ((rest (subseq buffer i)))
        (setf (fill-pointer buffer) 0)
        (loop for b across rest do (vector-push-extend b buffer))))
    (nreverse out)))

(defun flush-input (decoder)
  "Release what DECODER holds as plain keys (a lone ESC is the Escape key).
Call it when no more bytes arrived within a short timeout. → messages."
  (let ((buffer (input-decoder-buffer decoder)) (out '()))
    (when (and (plusp (length buffer)) (not (input-decoder-paste decoder)))
      (loop for b across buffer
            do (push (if (= b 27) (make-key-message :key :escape) (%control-key b)) out))
      (setf (fill-pointer buffer) 0))
    (nreverse out)))
