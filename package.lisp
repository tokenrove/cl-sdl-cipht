
(defpackage #:net.cipht/sdl2
  (:nicknames #:sdl)
  (:use #:cl #:cffi #:alexandria)
  (:export
   #:init #:quit #:with-init

   ;; VIDEO
   #:rect #:color #:pixel-format #:palette #:success?
   #:surface #:window #:texture #:renderer

   #:with-format
   #:with-rect-from-list
   #:with-rect-boa

   #:with-window-and-renderer
   #:render-set-logical-size
   #:set-render-draw-color
   #:render-clear
   #:render-present
   #:render-copy

   #:create-rgb-surface
   #:free-surface
   #:set-color-key
   #:with-locked-surface
   #:blit-surface
   #:fill-rect
   #:draw-pixel
   #:set-surface-palette
   #:palette-of
   #:width-of
   #:height-of
   #:format-of

   #:create-texture
   #:create-texture-from-surface
   #:update-texture
   #:update-texture-from-surface
   #:destroy-texture

   ;; TIMER
   #:get-ticks #:delay

   ;; EVENT
   #:pump-events #:poll-event #:wait-event
   #:with-event

   #:event-type #:event-keysym

   #:get-mouse-state #:warp-mouse-in-window
   #:get-mod-state #:set-mod-state
   #:show-cursor #:hide-cursor))

(defpackage #:net.cipht/sdl2-image
  (:nicknames #:sdl-image)
  (:use #:cl #:cffi)
  (:shadow #:load)
  (:export #:init #:quit #:with-init
           #:load #:load-texture
           #:save-png))

(defpackage #:net.cipht/sdl2-ttf
  (:nicknames #:sdl-ttf)
  (:use #:cl #:cffi)
  (:export #:with-init
           #:with-font
           #:render-text-solid
           #:render-text-blended
           #:render-text-shaded))
