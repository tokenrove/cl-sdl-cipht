
(defpackage #:cipht/sdl
  (:nicknames #:sdl)
  (:use #:cl #:cffi #:anaphora #:alexandria)
  (:export
   #:init #:quit #:with-init
   #:set-video-mode #:gl-swap-buffers #:gl-set-attribute #:list-modes
   #:display-width #:display-height
   #:get-ticks #:delay
   #:pump-events #:poll-event
   #:key-pressed? #:get-mouse-state #:warp-mouse
   #:get-mod-state #:set-mod-state #:modifier? #:get-simple-mod-state
   #:show-cursor #:hide-cursor
   #:num-joysticks #:joystick-name #:joystick-open #:joystick-close #:joystick-update
   #:joystick-get-axis #:joystick-get-button
   ))
