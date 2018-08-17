#|
 This file is a part of audio-blender
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#


(asdf:defsystem audio-blender
  :version "1.0.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Audio-blender allows you to mix and process raw audio samples."
  :homepage "https://Shirakumo.github.io/audio-blender/"
  :bug-tracker "https://github.com/Shirakumo/audio-blender/issues"
  :source-control (:git "https://github.com/Shirakumo/audio-blender.git")
  :serial T
  :components ((:file "package")
               (:file "dispatch")
               (:file "type-conversion")
               (:file "mixer-functions")
               (:file "channel")
               (:file "mixer")
               (:file "documentation"))
  :depends-on (:cffi
               :trivial-features
               :trivial-garbage
               :bordeaux-threads
               :documentation-utils))
