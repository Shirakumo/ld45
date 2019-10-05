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

(define-handler (active-editor save-world) (ev)
  (unless (packet +world+)
    (with-packet (packet (merge-pathnames
                          (query "Please enter the world save path" :parse #'uiop:native-namestring)
                          (pool-path 'ld45 NIL))
                         :direction :input)
      (setf (packet +world+) packet)))
  (save-world +world+ T :version T))

(define-handler (active-editor load-world) (ev)
  (change-scene (handler *context*) (load-world +world+)))
