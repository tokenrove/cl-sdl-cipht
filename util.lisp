(in-package #:net.cipht/sdl2)

(defun make-bitfield (type flags)
  (reduce #'logior (if (atom flags) (list flags) flags)
          :key (alexandria:curry #'foreign-enum-value type)
          :initial-value 0))

(defun maybe-null-ptr (ptr) (unless (null-pointer-p ptr) ptr))

(define-foreign-type success? ()
  ()
  (:actual-type :int)
  (:simple-parser success?))

(defmethod expand-to-foreign (value (type success?)) `(if ,value 0 -1))
(defmethod expand-from-foreign (value (type success?)) `(zerop ,value))
