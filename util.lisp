(in-package #:cipht/sdl)

;; from cl-opengl
(defun make-bitfield (enum-name attributes)
  (apply #'logior 0 (mapcar (lambda (x) (if x (foreign-enum-value enum-name x) 0)) attributes)))
(defun expand-enum-flags (type flags)
  (make-bitfield type (if (atom flags) (list flags) flags)))

