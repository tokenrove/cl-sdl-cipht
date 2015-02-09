;;;; -*- Lisp -*-

(in-package :asdf-user)

(defsystem cl-sdl-cipht
    :depends-on (:cffi :alexandria)
    :serial t
    :components
    ((:file "package")
     (:file "util")
     (:file "sdl")
     (:file "video")
     (:file "keys")
     (:file "events")
     (:file "timer")
     (:file "image")
     (:file "ttf")))
