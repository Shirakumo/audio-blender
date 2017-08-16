#|
 This file is a part of audio-blender
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#


(asdf:defsystem audio-blender-example
  :version "1.0.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Example application for the audio-blender using cl-mpg123 and cl-out123."
  :homepage "https://github.com/Shirakumo/audio-blender"
  :serial T
  :components ((:file "example"))
  :depends-on (:audio-blender
               :cl-mpg123
               :cl-out123))
