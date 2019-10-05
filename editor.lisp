(in-package #:org.shirakumo.fraf.ld45)

(define-subject editor (located-entity)
  ((name :initform :editor)))

(define-subject inactive-editor (editor)
  ())

(defmethod active-p ((_ inactive-editor)) NIL)

(define-subject active-editor (editor)
  ())

(defmethod active-p ((_ active-editor)) T)
