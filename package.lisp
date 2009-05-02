
(defpackage #:cipht/sdl
  (:nicknames #:sdl)
  (:use #:cl #:cffi #:anaphora)
  (:export
   #:init #:quit #:with-init
   #:set-video-mode #:gl-swap-buffers #:gl-set-attribute
   #:display-width #:display-height
   #:get-ticks #:delay
   #:pump-events #:poll-event
   #:key-pressed? #:get-mouse-state #:warp-mouse
   #:show-cursor #:hide-cursor
   ))
