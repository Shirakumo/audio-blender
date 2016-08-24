#|
 This file is a part of audio-blender
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(asdf:defsystem audio-blender
  :version "1.0.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Audio-blender allows you to mix and process raw audio samples."
  :homepage "https://github.com/Shirakumo/cl-soundio"
  :serial T
  :components ((:file "package")
               (:file "type-conversion")
               (:file "audio-blender")
               (:file "documentation"))
  :depends-on (:cffi
               :trivial-features
               :trivial-garbage
               :bordeaux-threads
               :documentation-utils))
