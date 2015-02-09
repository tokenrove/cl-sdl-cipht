;;;; TIMER

(in-package :net.cipht/sdl2)

(declaim (inline get-ticks delay))
(defcfun ("SDL_GetTicks" get-ticks) :uint32)
(defcfun ("SDL_Delay" delay) :void (ticks :uint32))
