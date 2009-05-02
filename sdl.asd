;;;; -*- Lisp -*-

(defpackage #:sdl-system (:use #:cl #:asdf))
(in-package #:sdl-system)

(defsystem sdl
    :depends-on (:cffi :anaphora)
    :serial t
    :components
    ((:file "package")
     (:file "sdl")))

