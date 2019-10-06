(in-package #:org.shirakumo.fraf.ld45)

(define-action editor-command ())

(define-action toggle-editor (editor-command)
  (key-press (one-of key :section)))

(define-action select-entity (editor-command)
  (key-press (one-of key :tab)))

(define-action delete-entity (editor-command)
  (key-press (one-of key :delete)))

(define-action place-wall (editor-command)
  (key-press (one-of key :f5)))

(define-action place-player (editor-command)
  (key-press (one-of key :f6)))

(define-action place-guard (editor-command)
  (key-press (one-of key :f7)))

(define-action loop-guard-path (editor-command)
  (key-press (one-of key :l)))

(define-action reverse-guard-path (editor-command)
  (key-press (one-of key :r)))

(define-action finish-guard-path (editor-command)
  (key-press (one-of key :enter)))

(define-action save-world (editor-command)
  (key-press (one-of key :f1)))

(define-action load-world (editor-command)
  (key-press (one-of key :f2)))

(define-action player-action ())

(define-action start-left (player-action)
  (key-press (one-of key :a :left))
  (gamepad-move (one-of axis :dpad-h) (< pos -0.4 old-pos)))

(define-action start-right (player-action)
  (key-press (one-of key :d :right))
  (gamepad-move (one-of axis :dpad-h) (< old-pos 0.4 pos)))

(define-action start-up (player-action)
  (key-press (one-of key :w :up))
  (gamepad-move (one-of axis :dpad-v) (< pos -0.4 old-pos)))

(define-action start-down (player-action)
  (key-press (one-of key :s :down))
  (gamepad-move (one-of axis :dpad-v) (< old-pos 0.8 pos)))

(define-action end-left (player-action)
  (key-release (one-of key :a :left))
  (gamepad-move (one-of axis :dpad-h) (< old-pos -0.4 pos)))

(define-action end-right (player-action)
  (key-release (one-of key :d :right))
  (gamepad-move (one-of axis :dpad-h) (< pos 0.4 old-pos)))

(define-action end-up (player-action)
  (key-release (one-of key :w :up))
  (gamepad-move (one-of axis :dpad-v) (< old-pos -0.4 pos)))

(define-action end-down (player-action)
  (key-release (one-of key :s :down))
  (gamepad-move (one-of axis :dpad-v) (< pos 0.8 old-pos)))

(define-retention movement (ev)
  (typecase ev
    (start-left (setf (retained 'movement :left) T))
    (start-right (setf (retained 'movement :right) T))
    (start-up (setf (retained 'movement :up) T))
    (start-down (setf (retained 'movement :down) T))
    (end-left (setf (retained 'movement :left) NIL))
    (end-right (setf (retained 'movement :right) NIL))
    (end-up (setf (retained 'movement :up) NIL))
    (end-down (setf (retained 'movement :down) NIL))))

(define-action attempt-takedown (player-action)
  (key-press (one-of key :f))
  (gamepad-press (one-of button :x)))

(define-action toggle-dragging (player-action)
  (key-press (one-of key :e))
  (gamepad-press (one-of button :b)))

(define-action aim (player-action)
  (key-press (one-of key :v))
  (gamepad-press (one-of button :a)))

(define-action shoot (player-action)
  (key-release (one-of key :v))
  (gamepad-release (one-of button :a)))
