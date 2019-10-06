(defsystem ld45
  :author "Nicolas Hafner <shinmera@tymoon.eu>, Joram Schrijver <i@joram.io>"
  :homepage "https://github.com/shirakumo/ld45"
  :source-control (:git "https://github.com/shirakumo/ld45")
  :license "zlib"
  :components ((:file "package")
               (:file "toolkit")
               (:file "keys")
               (:file "packet")
               (:file "serialization")
               (:file "pathfinding")
               (:file "world")
               (:file "camera")
               (:file "collision")
               (:file "wall")
               (:file "moving")
               (:file "viewcone")
               (:file "viewcone2")
               (:file "guard")
               (:file "player")
               (:file "editor")
               (:file "main")
               (:file "v0"))
  :defsystem-depends-on (:deploy)
  :build-operation "deploy-op"
  :build-pathname "ld45"
  :entry-point "org.shirakumo.fraf.ld45:launch"
  :depends-on (:trial-glfw
               :zip
               :fast-io
               :ieee-floats
               :babel
               :alexandria
               :trivial-indent))
