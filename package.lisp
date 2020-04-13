(defpackage #:org.shirakumo.fraf.ld45
  (:nicknames #:ld45)
  (:use #:cl+trial)
  (:shadow #:launch #:main #:camera #:step #:located-entity)
  (:local-nicknames
   (#:cl-gamepad #:org.shirakumo.fraf.gamepad))
  (:export
   #:launch))
