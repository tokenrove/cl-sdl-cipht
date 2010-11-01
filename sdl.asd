;;;; -*- Lisp -*-

(defpackage #:sdl-system (:use #:cl #:asdf))
(in-package #:sdl-system)

(defsystem sdl
    :depends-on (:cffi :alexandria)
    :serial t
    :components
    ((:file "package")
     (:file "util")
     (:file "sdl")))

