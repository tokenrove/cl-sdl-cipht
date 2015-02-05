
(in-package :net.cipht/sdl2-image)

(define-foreign-library sdl-image
  (:windows "SDL2_image")
  (t (:default "libSDL2_image")))
(use-foreign-library sdl-image)

(defcenum init-flags
  (:init-jpg #x1)
  (:init-png #x2)
  (:init-tif #x4)
  (:init-webp #x8))
(defcfun ("IMG_Init" %init) sdl:success? (flags init-flags))
(defcfun ("IMG_Quit" quit) :void)

(defun init (flags) (%init (sdl::make-bitfield 'init-flags flags)))

(defmacro with-init ((&optional (mask :init-png)) &body body)
  `(unwind-protect (progn (init ,mask) ,@body)
     (quit)))

(defcfun ("IMG_Load" %load) (:pointer (:struct sdl:surface)) (path :string))
(defun load (path) (sdl::nil<-null (%load path)))

(defcfun ("IMG_LoadTexture" %load-texture)
    sdl:texture
  (renderer sdl:renderer)
  (path :string))
(defun load-texture (renderer path)
  (sdl::nil<-null (%load-texture renderer path)))

(defcfun ("IMG_SavePNG" save-png)
    sdl:success?
  (surface (:pointer (:struct sdl:surface)))
  (path :string))
