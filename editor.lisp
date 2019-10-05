(in-package #:org.shirakumo.fraf.ld45)

(define-subject editor (located-entity)
  ((name :initform :editor)))

(define-subject inactive-editor (editor)
  ())

(defmethod active-p ((_ inactive-editor)) NIL)

(define-handler (inactive-editor toggle-editor) (ev)
  (change-class inactive-editor 'active-editor))

(define-subject active-editor (editor)
  ())

(defmethod active-p ((_ active-editor)) T)

(define-handler (active-editor toggle-editor) (ev)
  (change-class active-editor 'inactive-editor))
