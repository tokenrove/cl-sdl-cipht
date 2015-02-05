
(defpackage #:net.cipht/sdl2
  (:nicknames #:sdl)
  (:use #:cl #:cffi #:alexandria)
  (:export
   #:init #:quit #:with-init

   ;; VIDEO
   #:surface #:rect #:pixel-format #:palette

   #:with-window-and-renderer
   #:render-set-logical-size
   #:set-render-draw-color
   #:render-clear
   #:render-present
   #:render-copy

   #:create-rgb-surface
   #:free-surface
   #:set-color-key
   #:lock-surface
   #:unlock-surface
   #:blit-surface
   #:fill-rect

   #:create-texture
   #:create-texture-from-surface
   #:update-texture

   ;; TIMER
   #:get-ticks #:delay

   ;; EVENT
   #:pump-events #:poll-event #:wait-event
   #:with-event #:event-type
   #:get-mouse-state #:warp-mouse-in-window
   #:get-mod-state #:set-mod-state
   #:show-cursor #:hide-cursor
   ))

(defpackage #:net.cipht/sdl2-image
  (:nicknames #:sdl-image)
  (:use #:cl #:cffi)
  (:shadow #:load)
  (:export #:init #:quit #:with-init
           #:load #:load-texture
           #:save-png))
