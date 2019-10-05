(in-package #:org.shirakumo.fraf.ld45)

(define-action editor-command ())

(define-action toggle-editor (editor-command)
  (key-press (one-of key :section)))
