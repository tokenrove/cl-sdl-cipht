
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

;;;; VIDEO

(defcenum window-flags
  (:window-fullscreen #x00000001)  ; fullscreen window
  (:window_opengl #x00000002D)     ; window usable with OpenGL context
  (:window-shown #x00000004)       ; window is visible
  (:window-hidden #x00000008)      ; window is not visible
  (:window-borderless #x00000010)  ; no window decoration
  (:window-resizable #x00000020)   ; window can be resized
  (:window-minimized #x00000040)   ; window is minimized
  (:window-maximized #x00000080)   ; window is maximized
  (:window-input-grabbed #x00000100)  ; window has grabbed input focus
  (:window-input-focus #x00000200)    ; window has input focus
  (:window-mouse-focus #x00000400)    ; window has mouse focus
  (:window-fullscreen-desktop #x00001001)
  (:window-foreign #x00000800)        ; window not created by SDL
  (:window-allow-highdpi #x00002000)) ; window should be created in high-DPI mode if supported

(defcenum gl-attribute
  :gl-red-size
  :gl-green-size
  :gl-blue-size
  :gl-alpha-size
  :gl-buffer-size
  :gl-doublebuffer
  :gl-depth-size
  :gl-stencil-size
  :gl-accum-red-size
  :gl-accum-green-size
  :gl-accum-blue-size
  :gl-accum-alpha-size
  :gl-stereo
  :gl-multisamplebuffers
  :gl-multisamplesamples
  :gl-accelerated-visual
  :gl-retained-backing
  :gl-context-major-version
  :gl-context-minor-version
  :gl-context-egl
  :gl-context-flags
  :gl-context-profile-mask
  :gl-share-with-current-context
  :gl-framebuffer-srgb-capable)

(defcstruct color
  (r :uint8)
  (g :uint8)
  (b :uint8)
  (a :uint8))

(defcstruct palette
  (ncolors :int)
  (colors (:pointer (:struct color)))
  (version :uint32)
  (refcount :int))

(defcstruct point
  (x :int)
  (y :int))

(defcstruct rect
  (x :int)
  (y :int)
  (w :int)
  (h :int))

(defmacro with-rect-from-list ((var list) &body body)
  `(cffi:with-foreign-object (,var '(:struct rect))
     (loop for slot in '(x y w h)
           for value in ,list
           do (setf (foreign-slot-value ,var '(:struct rect) slot) value))
     ,@body))
(defmacro with-rect-boa ((var x y w h) &body body)
  `(cffi:with-foreign-object (,var '(:struct rect))
     (loop for slot in '(x y w h)
           for value in (list ,x ,y ,w ,h)
           do (setf (foreign-slot-value ,var '(:struct rect) slot) value))
     ,@body))

(defcenum pixel-formats
  (:pixel-format-argb8888 #x16362004))

(defcstruct pixel-format
  (format pixel-formats)
  (palette (:pointer (:struct palette)))
  (bits-per-pixel :uint8)
  (bytes-per-pixel :uint8)
  (padding :uint8 :count 2)
  (rmask :uint32)
  (gmask :uint32)
  (bmask :uint32)
  (amask :uint32)
  (rloss :uint8)
  (gloss :uint8)
  (bloss :uint8)
  (aloss :uint8)
  (rshift :uint8)
  (gshift :uint8)
  (bshift :uint8)
  (ashift :uint8)
  (refcount :int)
  (next :pointer))                      ; to :struct pixel-format


(defcfun ("SDL_AllocFormat" alloc-format)
    (:pointer (:struct pixel-format))
  (format pixel-formats))
(defcfun ("SDL_FreeFormat" free-format)
    :void
  (format (:pointer (:struct pixel-format))))

(defmacro with-format ((var format) &body body)
  `(let ((,var (alloc-format ,format)))
     (unwind-protect (progn ,@body)
       (free-format ,var))))

(defcenum surface-flags
  (:software-surface #x0)
  (:prealloc #x1)
  (:rle-accel #x2)
  (:dont-free #x4))

(defcstruct surface
  (flags surface-flags)
  (format (:pointer (:struct pixel-format)))
  (w :int)
  (h :int)
  (pitch :int)
  (pixels :pointer)
  (userdata :pointer)
  (locked :int)
  (lockdata :pointer)
  (clip-rect (:struct rect))
  (map :pointer)                        ; (:struct blit-map)
  (refcount :int))

(defcenum renderer-flags
  (:renderer-software #x00000001) ; The renderer is a software fallback
  (:renderer-accelerated #x00000002) ; The renderer uses hardware acceleration
  (:renderer-presentvsync #x00000004) ; Present is synchronized with the refresh rate
  (:renderer-targettexture #x00000008)) ; The renderer supports rendering to texture

(defctype window :pointer)
(defctype renderer :pointer)
(defctype texture :pointer)

(defcfun ("SDL_CreateWindowAndRenderer" create-window-and-renderer)
    :int
  (width :int)
  (height :int)
  (window-flags window-flags)
  (window (:pointer window))
  (renderer (:pointer renderer)))

(defcfun ("SDL_DestroyWindow" destroy-window) :void (window window))
(defcfun ("SDL_DestroyRenderer" destroy-renderer) :void (renderer renderer))

(defmacro with-window-and-renderer ((window-var renderer-var width height flags) &body body)
  (with-gensyms (window-ptr renderer-ptr)
    `(with-foreign-objects ((,window-ptr 'window)
                            (,renderer-ptr 'renderer))
       (unwind-protect
            (progn
              (create-window-and-renderer ,width ,height ,flags
                                          ,window-ptr
                                          ,renderer-ptr)
              (let ((,window-var (mem-ref ,window-ptr :pointer))
                    (,renderer-var (mem-ref ,renderer-ptr :pointer)))
                ,@body))
         (destroy-renderer (mem-ref ,renderer-ptr :pointer))
         (destroy-window (mem-ref ,window-ptr :pointer))))))

(defcfun ("SDL_GetWindowID" get-window-id) :uint32 (window window))

(defcfun ("SDL_SetWindowTitle" set-window-title)
    :void
  (window :pointer)
  (title :string))

(defcfun ("SDL_GetWindowTitle" get-window-title)
    :string
  (window :pointer))

(defcfun ("SDL_RenderSetLogicalSize" render-set-logical-size)
    success?
  (renderer renderer)
  (width :int)
  (height :int))

(defcfun ("SDL_SetRenderDrawColor" set-render-draw-color)
    success?
  (renderer renderer)
  (r :uint8) (g :uint8) (b :uint8) (a :uint8))

(defcfun ("SDL_RenderClear" render-clear) success? (renderer renderer))

(defcfun ("SDL_RenderPresent" render-present) :void (renderer renderer))

(defcfun ("SDL_RenderCopy" %render-copy)
    success?
  (renderer renderer)
  (texture texture)
  (srcrect (:pointer (:struct rect)))
  (dstrect (:pointer (:struct rect))))

(defun render-copy (renderer texture srcrect dstrect)
  (%render-copy renderer texture (null<-nil srcrect) (null<-nil dstrect)))

(defcfun ("SDL_CreateRGBSurface" %create-rgb-surface)
    (:pointer (:struct surface))
  (flags :uint32)
  (width :int)
  (height :int)
  (depth :int)
  (rmask :uint32)
  (gmask :uint32)
  (bmask :uint32)
  (amask :uint32))

(defun create-rgb-surface (width height format)
  (with-foreign-slots ((bits-per-pixel rmask gmask bmask amask) format (:struct pixel-format))
    (nil<-null (%create-rgb-surface 0 width height bits-per-pixel rmask gmask bmask amask))))

(defcfun ("SDL_FreeSurface" free-surface)
    :void
  (surface (:pointer (:struct surface))))

(defcfun ("SDL_SetSurfacePalette" set-surface-palette)
    :int
  (surface (:pointer (:struct surface)))
  (palette (:pointer (:struct palette))))

(defun palette-of (surface)
  (let ((format (foreign-slot-value surface '(:struct surface) 'format)))
    (with-foreign-slots ((palette) format (:struct pixel-format))
      (nil<-null palette))))

(defun width-of (surface)
  (foreign-slot-value surface '(:struct surface) 'w))
(defun height-of (surface)
  (foreign-slot-value surface '(:struct surface) 'h))

(defcfun ("SDL_LockSurface" lock-surface)
    success?
  (surface (:pointer (:struct surface))))
(defcfun ("SDL_UnlockSurface" unlock-surface)
    success?
  (surface (:pointer (:struct surface))))

(defmacro with-locked-surface ((surface) &body body)
  `(unwind-protect
        (progn
          (lock-surface ,surface)
          ,@body)
     (unlock-surface ,surface)))

(defun draw-pixel (surface x y color)
  "Assumes SURFACE is already locked."
  (with-foreign-slots ((pixels pitch format) surface (:struct surface))
    (destructuring-bind (type adjusted-pitch)
        (ecase (foreign-slot-value format '(:struct pixel-format) 'bits-per-pixel)
          (1 (list :uint8 pitch))
          (2 (list :uint16 (ash pitch -1)))
          (4 (list :uint32 (ash pitch -2))))
      (setf (mem-aref pixels type (+ x (* y adjusted-pitch))) color))))

(defcfun ("SDL_SetColorKey" set-color-key)
    success?
  (surface (:pointer (:struct surface)))
  (flag :boolean)
  (key :uint32))

(defcfun ("SDL_UpperBlit" %blit-surface)
    success?
  (src (:pointer (:struct surface)))
  (src-rect (:pointer (:struct rect)))
  (dst (:pointer (:struct surface)))
  (dst-rect (:pointer (:struct rect))))

(defun blit-surface (src src-rect dst x y)
  (with-foreign-object (dst-rect '(:struct rect))
    (setf (foreign-slot-value dst-rect '(:struct rect) 'x) x
          (foreign-slot-value dst-rect '(:struct rect) 'y) y)
    (%blit-surface src (null<-nil src-rect) dst dst-rect)))

(defcfun ("SDL_FillRect" %fill-rect)
    success?
  (dst (:pointer (:struct surface)))
  (rect (:pointer (:struct rect)))
  (color :uint32))

(defun fill-rect (dst rect color)
  (%fill-rect dst (null<-nil rect) color))

(defcenum texture-access-flags
  (:texture-access-static)
  (:texture-access-streaming)
  (:texture-access-target))

(defcfun ("SDL_CreateTexture" create-texture)
    texture
  (renderer renderer)
  (format pixel-formats)
  (access texture-access-flags)
  (width :int)
  (height :int))

(defcfun ("SDL_CreateTextureFromSurface" create-texture-from-surface)
    texture
  (renderer renderer)
  (surface (:pointer (:struct surface))))

(defcfun ("SDL_UpdateTexture" %update-texture)
    success?
  (texture texture)
  (rect (:pointer (:struct rect)))
  (pixels :pointer)
  (pitch :int))
(defun update-texture (texture rect pixels pitch)
  (%update-texture texture (null<-nil rect) pixels pitch))
(defun update-texture-from-surface (texture rect surface)
  "Per UPDATE-TEXTURE, but get PIXELS and PITCH from SURFACE."
  (with-foreign-slots ((pixels pitch) surface (:struct surface))
    (update-texture texture rect pixels pitch)))

(defcfun ("SDL_DestroyTexture" destroy-texture) :void (texture texture))

;;;; EVENTS

(defcenum event-type
  (:noevent 0)			  ; Unused (do not remove)
  (:quit #x100)                   ; User-requested quit
  (:window-event #x200)           ; Window state change
  (:syswm-event #x201)            ; System-specific event
  (:key-down #x300)               ; Keys pressed
  :key-up			  ; Keys released
  :text-editing
  :text-input
  (:mouse-motion #x400)           ; Mouse moved
  :mouse-button-down		  ; Mouse button pressed
  :mouse-button-up		  ; Mouse button released
  :mouse-wheel
  (:joyaxismotion #x600)          ; Joystick axis motion
  :joy-ball-motion		  ; Joystick trackball motion
  :joy-hat-motion                 ; Joystick hat position change
  :joy-button-down		  ; Joystick button pressed
  :joy-button-up                  ; Joystick button released
  :joy-device-added
  :joy-device-removed
  (:controller-axis-motion #x650)
  :controller-button-down
  :controller-button-up
  :controller-device-added
  :controller-device-removed
  :controller-device-remapped
  ;; Events SDL-USEREVENT through SDL-LASTEVENT are for your use
  (:userevent #x8000))

(defcstruct keysym
  (scancode :uint8)
  (sym key-code)
  (mod mod)
  (unicode :uint16))

(defcstruct keyboard-event
  (type :uint32)
  (timestamp :uint32)
  (window-id :uint32)
  (state :uint8)
  (repeat :uint8)
  (padding :uint8 :count 2)
  (keysym (:struct keysym)))

(defcstruct mouse-motion-event
  (type :uint32)
  (timestamp :uint32)
  (window-id :uint32)
  (which :uint32)
  (state :uint32)
  (x :int32)
  (y :int32)
  (xrel :int32)
  (yrel :int32))

(defcstruct mouse-button-event
  (type :uint32)
  (timestamp :uint32)
  (window-id :uint32)
  (which :uint32)
  (button :uint8)
  (state :uint8)
  (clicks :uint8)
  (padding :uint8)
  (x :int32)
  (y :int32))

(defcstruct user-event
  (type :uint32)
  (timestamp :uint32)
  (window-id :uint32)
  (code :int32)
  (data1 :pointer)
  (data2 :pointer))

(defcstruct resize-event
  (type :uint8)
  (w :int)
  (h :int))

(defcstruct common-event
  (type :uint32)
  (timestamp :uint32))

(defcunion event
  (type :uint32)
  (common (:struct common-event))
  (key (:struct keyboard-event))
  (motion (:struct mouse-motion-event))
  (button (:struct mouse-button-event))
  (user (:struct user-event))
  (padding :uint8 :count 56))

(declaim (inline pump-events poll-event event-type wait-event event-state))
(defcfun ("SDL_PumpEvents" pump-events) :void)
;; True if there are more events
(defcfun ("SDL_PollEvent" poll-event) :boolean (event (:pointer (:union event))))
;; Should signal a condition if return value is 0
(defcfun ("SDL_WaitEvent" wait-event) :int (event (:pointer (:union event))))
(defcenum event-states
  (:sdl-ignore 0)
  (:sdl-enable 1)
  (:sdl-query -1))
(defcfun ("SDL_EventState" event-state) :uint8 (type :uint32) (state event-states))

(defmacro with-event ((event-variable) &body body)
  `(with-foreign-object (,event-variable '(:union event))
     ,@body))

(defun event-type (event)
  (let ((sv (foreign-slot-value (mem-ref event '(:pointer (:union event)))
                                '(:union event)
                                'type)))
   (foreign-enum-keyword 'event-type sv :errorp nil)))

(defun event-keysym (event)
  (let ((keysym
          (foreign-slot-value (mem-ref event '(:pointer (:struct keyboard-event)))
                      '(:struct keyboard-event)
                      'keysym)))
    (foreign-slot-value keysym '(:struct keysym) 'sym)))

(declaim (inline get-mod-state %get-mod-state set-mod-state))
(defcfun ("SDL_GetModState" get-mod-state) mod)
(defcfun ("SDL_GetModState" %get-mod-state) :uint8)
(defcfun ("SDL_SetModState" set-mod-state) :void (mod mod))

(declaim (inline %get-mouse-state get-mouse-state %show-cursor warp-mouse))
(defcfun ("SDL_GetMouseState" %get-mouse-state) :uint8 (x :pointer) (y :pointer))

(defun get-mouse-state ()
  (with-foreign-objects ((x :int) (y :int))
    (let ((buttons (%get-mouse-state x y)))
      (values buttons (mem-ref x :int) (mem-ref y :int)))))

(defcfun ("SDL_ShowCursor" %show-cursor) :int (toggle :int))

(defun show-cursor () (%show-cursor 1))
(defun hide-cursor () (%show-cursor 0))

(defcfun ("SDL_WarpMouseInWindow" warp-mouse-in-window)
    :void
  (window window)
  (x :int)
  (y :int))

;;;; TIMER

(declaim (inline get-ticks delay))
(defcfun ("SDL_GetTicks" get-ticks) :uint32)
(defcfun ("SDL_Delay" delay) :void (ticks :uint32))
