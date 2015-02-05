;;;; -*- Lisp -*-

(in-package :asdf-user)

(defsystem cl-sdl-cipht
    :depends-on (:cffi :alexandria)
    :serial t
    :components
    ((:file "package")
     (:file "util")
     (:file "keys")
     (:file "sdl")
     (:file "image")
     (:file "ttf")))
