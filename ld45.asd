(defsystem ld45
  :author "Nicolas Hafner <shinmera@tymoon.eu>, Joram Schrijver <i@joram.io>"
  :homepage "https://github.com/shirakumo/ld45"
  :source-control (:git "https://github.com/shirakumo/ld45")
  :license "zlib"
  :components ((:file "package")
               (:file "toolkit")
               (:file "world")
               (:file "camera")
               (:file "player")
               (:file "editor")
               (:file "main"))
  :defsystem-depends-on (:deploy)
  :build-operation "deploy-op"
  :build-pathname "ld45"
  :entry-point "org.shirakumo.fraf.ld45:launch"
  :depends-on (:trial-glfw))
