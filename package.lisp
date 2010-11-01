
(defpackage #:cipht/sdl
  (:nicknames #:sdl)
  (:use #:cl #:cffi #:alexandria)
  (:export
   #:init #:quit #:with-init
   #:set-video-mode #:gl-swap-buffers #:gl-set-attribute #:list-modes
   #:display-width #:display-height
   #:get-ticks #:delay
   #:pump-events #:poll-event #:wait-event
   #:event-loop #:event-type
   #:key-pressed? #:get-mouse-state #:warp-mouse
   #:get-mod-state #:set-mod-state
   #:show-cursor #:hide-cursor
   #:num-joysticks #:joystick-name #:joystick-open #:joystick-close #:joystick-update
   #:joystick-get-axis #:joystick-get-button
   ))
