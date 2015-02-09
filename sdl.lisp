(in-package :net.cipht/sdl2)

(define-foreign-library sdl2
  (:unix (:or "libSDL2-2.0.so" "libSDL2"))
  (:windows "SDL2")
  (t (:default "libSDL2")))
(use-foreign-library sdl2)

;;;; INITIALIZATION

(defcenum init-enum
  (:init-timer #x00000001)
  (:init-audio #x00000010)
  (:init-video #x00000020)
  (:init-joystick #x00000200)
  (:init-haptic #x00001000)
  (:init-game-controller #x00002000)
  (:init-events #x00004000)
  (:init-no-parachute #x00100000)	; Don't catch fatal signals
  (:init-event-thread #x01000000)	; Not supported on all OS's
  (:init-everything #x0000FFFF))

(defcfun ("SDL_Init" init) success? (flags init-enum))
(defcfun ("SDL_Quit" quit) :void)

(defmacro with-init ((&optional (mask :init-everything)) &body body)
  `(unwind-protect (progn (sdl:init ,mask) ,@body)
     (sdl:quit)))
