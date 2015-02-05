
(in-package :net.cipht/sdl2-ttf)

(define-foreign-library sdl-ttf
  (:windows "SDL2_ttf")
  (t (:default "libSDL2_ttf")))
(use-foreign-library sdl-ttf)

(defcfun ("TTF_Init" init) sdl:success?)
(defcfun ("TTF_Quit" quit) :void)

(defmacro with-init (() &body body)
  `(unwind-protect (progn (init) ,@body)
     (quit)))

(defctype font :pointer)

(defcfun ("TTF_OpenFont" open-font) font (file :string) (ptsize :int))
(defcfun ("TTF_CloseFont" close-font) :void (font font))

(defmacro with-font ((var path ptsize) &body body)
  `(let ((,var (open-font ,path ,ptsize)))
     (unwind-protect (progn ,@body)
       (close-font ,var))))

;;; Here's another grotesque hack: passing structs by value is a huge
;;; pain in the ass, and we only need it for these SDL_Color
;;; parameters, so we just pass uint32 instead.  The alignment is
;;; probably wrong on some platforms, so be suspicious of this code.
(defcfun ("TTF_RenderText_Solid" render-text-solid)
    (:pointer (:struct sdl:surface))
  (font font) (text :string) (fg :uint32))

(defcfun ("TTF_RenderText_Blended" render-text-blended)
    (:pointer (:struct sdl:surface))
  (font font) (text :string) (fg :uint32))

(defcfun ("TTF_RenderText_Shaded" render-text-shaded)
    (:pointer (:struct sdl:surface))
  (font font) (text :string) (fg :uint32) (bg :uint32))
