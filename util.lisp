(in-package #:net.cipht/sdl2)

(defun make-bitfield (type flags)
  (reduce #'logior (if (atom flags) (list flags) flags)
          :key (alexandria:curry #'foreign-enum-value type)
          :initial-value 0))

;; Obviously we should use a type translator instead of calling these
;; manually, but I didn't want to spend the extra time wrangling with
;; define-parse-method and so on.
(defun nil<-null (ptr) (unless (null-pointer-p ptr) ptr))
(defun null<-nil (value) (or value (null-pointer)))

(define-foreign-type success? ()
  ()
  (:actual-type :int)
  (:simple-parser success?))

(defmethod expand-to-foreign (value (type success?)) `(if ,value 0 -1))
(defmethod expand-from-foreign (value (type success?)) `(zerop ,value))
