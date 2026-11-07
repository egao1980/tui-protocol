# tui-protocol

Full-screen terminal programs for [cl-stack](https://github.com/egao1980/cl-stack), in the Elm architecture (as in bubbletea / cl-tuition). `tui-update` takes a model and a message and returns a new model. `tui-view` draws the model on a **canvas** of [`terminal-protocol`](https://github.com/egao1980/terminal-protocol) cells. Brief: [`cl-stack/docs/capabilities/tui.md`](https://github.com/egao1980/cl-stack/blob/main/docs/capabilities/tui.md) ([#274](https://github.com/egao1980/cl-stack/issues/274)).

```lisp
(defclass counter () ())
(defmethod stack-tui:tui-init ((p counter)) (values 0 nil))
(defmethod stack-tui:tui-update ((p counter) n (m stack-tui:key-message))
  (case (stack-tui:key-message-key m)
    (:equal (values (1+ n) nil))       ; "+"
    (:q (values n :quit))
    (t (values n nil))))
(defmethod stack-tui:tui-view ((p counter) n canvas)
  (stack-tui:draw-box canvas 0 0 (stack-tui:canvas-cols canvas) 3 :title "counter")
  (stack-tui:draw-text canvas 2 1 (format nil "count: ~D" n)))

(stack-tui:run-tui (make-instance 'counter) :backend …)
```

| Piece | What it does |
|-------|--------------|
| Canvas | `draw-text` handles wide characters, combining marks and clipping. Also `fill-rect`, `draw-box`, and `sub-canvas` (a clipped view). |
| `render-diff` / `render-full` | Canvas → VT bytes: only changed cells, minimal SGR and cursor moves, ECH for blank runs, OSC 8 links, optional DEC 2026 synchronized output |
| `decode-input` | Terminal bytes → `key-message` / `mouse-message` / `paste-message` / `focus-message`. Covers legacy and xterm-modified keys, SS3, CSI-u, `modifyOtherKeys`, SGR and X10 mouse, and bracketed paste. Partial sequences are held across reads; `flush-input` releases a lone ESC. |
| `run-tui` | The loop: draw, read, decode, update. Commands are `:quit`, `(tick s)`, `(task fn)` (runs off-thread, and its result comes back as a message) and `(batch …)`. `tui-send` works from any thread. The terminal is restored even when `tui-update` signals. |

Backends supply the bytes: `tui-backend-terminal` (headless, on any `terminal-protocol` emulator) and `tui-backend-tty` (a real terminal).

**How it's tested:**
- The renderer's output is fed through `terminal-backend-lisp`, and the emulator's screen must equal the canvas, cell for cell.
- Every byte sequence the emulator's `encode-key` / `encode-mouse` / `encode-paste` / `encode-focus` produce must decode back to the same event, fed whole and fed byte by byte.

## License

MIT
