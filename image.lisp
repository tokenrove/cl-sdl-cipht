
(in-package #:cipht/sdl-image)

(define-foreign-library sdl
  (:windows "SDL_image")
  (t (:default "libSDL_image")))
(use-foreign-library sdl)

(defcenum init-flags
  (:init-jpg #x1)
  (:init-png #x2)
  (:init-tif #x4))
(defcfun ("IMG_Init" %init) :boolean (flags :uint32))
(defcfun ("IMG_Quit" quit) :void)

(defun init (flags) (%init (cipht/sdl::expand-enum-flags 'init-flags flags)))

(defmacro with-init ((&optional (mask :init-png)) &body body)
  `(unwind-protect (progn (cipht/sdl-image:init ,mask) ,@body)
     (cipht/sdl-image:quit)))

(defcfun ("IMG_Load" %load) sdl::surface (path :string))
(defun load (path)
  (let ((result (%load path))) (unless (null-pointer-p result) result)))
