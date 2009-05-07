
(in-package :cipht/sdl)

(define-foreign-library sdl
  (t (:default "libSDL")))
(use-foreign-library sdl)

;;;; INITIALIZATION

(defcenum init-enum
  (:init-timer #x00000001)
  (:init-audio #x00000010)
  (:init-video #x00000020)
  (:init-cdrom #x00000100)
  (:init-joystick #x00000200)
  (:init-no-parachute #x00100000)	; Don't catch fatal signals
  (:init-event-thread #x01000000)	; Not supported on all OS's
  (:init-everything #x0000FFFF))

(defcfun ("SDL_Init" init) :boolean (flags init-enum))
(defcfun ("SDL_Quit" quit) :void)

(defmacro with-init ((&optional (mask :init-everything)) &body body)
  `(unwind-protect (progn (sdl:init ,mask) ,@body)
     (sdl:quit)))

;;;; VIDEO

(defcenum video-flags
  ;; These are the currently supported flags for the SDL_surface
  ;; Available for SDL_CreateRGBSurface() or SDL_SetVideoMode()
  (:swsurface #x00000000)	  ; Surface is in system memory
  (:hwsurface #x00000001)	  ; Surface is in video memory
  (:asyncblit #x00000004)	  ; Use asynchronous blits if possible
  ;; Available for SDL_SetVideoMode()
  (:anyformat #x10000000)	  ; Allow any video depth/pixel-format
  (:hwpalette #x20000000)	  ; Surface has exclusive palette
  (:doublebuf #x40000000)	  ; Set up double-buffered video mode
  (:fullscreen #x80000000)	 ; Surface is a full screen display
  (:opengl #x00000002)		 ; Create an OpenGL rendering context
  (:openglblit #x0000000A) ; Create an OpenGL rendering context and use it for blitting
  (:resizable #x00000010)  ; This video mode may be resized
  (:noframe #x00000020)	   ; No window caption or edge frame
  ;; Used internally (read-only)
  (:hwaccel #x00000100)		  ; Blit uses hardware acceleration
  (:srccolorkey #x00001000)	  ; Blit uses a source color key
  (:rleaccelok #x00002000)	  ; Private flag
  (:rleaccel #x00004000)	  ; Surface is RLE encoded
  (:srcalpha #x00010000)	  ; Blit uses source alpha blending
  (:prealloc #x01000000))	  ; Surface uses preallocated memory

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
  :gl-swap-control)

(defcstruct pixel-format
  (palette :pointer)
  (bits-per-pixel :uint8)
  (bytes-per-pixel :uint8)
  (r-loss :uint8)
  (g-loss :uint8)
  (b-loss :uint8)
  (a-loss :uint8)
  (r-shift :uint8)
  (g-shift :uint8)
  (b-shift :uint8)
  (a-shift :uint8)
  (r-mask :uint32)
  (g-mask :uint32)
  (b-mask :uint32)
  (a-mask :uint32)
  (colorkey :uint32)
  (alpha  :uint8))

(defcstruct rect
  (x :int16)
  (y :int16)
  (w :uint16)
  (h :uint16))

(defcstruct surface
  (flags :uint32)
  (format :pointer)
  (w :int)
  (h :int)
  (pitch :uint16)
  (pixels :pointer)
  (offset :int)
  (hwdata :pointer)
  (clip-rect rect)
  (unused1 :uint32)
  (map :pointer)
  (format-version :uint)
  (refcount :int))



;; from cl-opengl
(defun make-bitfield (enum-name attributes)
  (apply #'logior 0 (mapcar (lambda (x) (if x (foreign-enum-value enum-name x) 0)) attributes)))
(defun expand-video-flags (flags)
  (make-bitfield 'video-flags (if (atom flags) (list flags) flags)))

(defcfun ("SDL_ListModes" %list-modes) :pointer (format :pointer) (flags :uint32))

(defun list-modes (flags &key (format (null-pointer)))
  (let ((modes (%list-modes format (expand-video-flags flags))))
    (if (= #xffffffff (pointer-address modes)) nil
	(loop for i from 0
	      for mode = (mem-aref modes :pointer i)
	      while (and mode (not (null-pointer-p mode)))
	      collect (with-foreign-slots ((w h) mode rect) (list w h))))))

(defcfun ("SDL_SetVideoMode" %set-video-mode) :pointer (width :int) (height :int) (bpp :int) (flags :uint32))

(defun set-video-mode (width height bpp flags)
  (%set-video-mode width height bpp (expand-video-flags flags)))

(defcfun ("SDL_GL_SwapBuffers" gl-swap-buffers) :void)
(defcfun ("SDL_GL_GetAttribute" gl-get-attribute) :boolean (attribute gl-attribute) (value :pointer))
(defcfun ("SDL_GL_SetAttribute" gl-set-attribute) :boolean (attribute gl-attribute) (value :int))

(defcfun ("SDL_GetVideoSurface" get-video-surface) surface)

(defun display-width () (foreign-slot-value (get-video-surface) 'surface 'w))
(defun display-height () (foreign-slot-value (get-video-surface) 'surface 'h))

;;;; EVENTS

;; Keyboard key definitions: 8-bit ISO-8859-1 (Latin 1) encoding is used
;; for printable keys (such as A-Z, 0-9 etc), and values above 256
;; represent special (non-printable) keys (e.g. F1, Page Up etc).
(define-foreign-type key-code-type ()
  ()
  (:actual-type :int)
  (:simple-parser key-code))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((symbol-table '((:unknown -1)
			(:up 273)
			(:down 274)
			(:right 275)
			(:left 276)
			(:insert 277)
			(:home 278)
			(:end 279)
			(:pageup 280)
			(:pagedown 281)
			(:f1 282)
			(:f2 283)
			(:f3 284)
			(:f4 285)
			(:f5 286)
			(:f6 287)
			(:f7 288)
			(:f8 289)
			(:f9 290)
			(:f10 291)
			(:f11 292)
			(:f12 293)
			(:f13 294)
			(:f14 295)
			(:f15 296)
			(:numlock 300)
			(:capslock 301)
			(:scrollock 302)
			(:rshift 303)
			(:lshift 304)
			(:rctrl 305)
			(:lctrl 306)
			(:ralt 307)
			(:lalt 308)
			(:rmeta 309)
			(:lmeta 310)
			(:lsuper 311)
			(:rsuper 312)
			(:mode 313)
			(:compose 314)
			(:help 315)
			(:print 316)
			(:sysreq 317)
			(:break 318)
			(:menu 319))))
    (defmethod expand-to-foreign (value (type key-code-type))
      `(cl:cond ((characterp ,value) (char-code ,value))
		((symbolp ,value) (ecase ,value
				    ,@symbol-table))
		(t ,value)))
    (defmethod expand-from-foreign (value (type key-code-type))
      `(case ,value
	 ,@(mapcar #'reverse symbol-table) ;; XXX probably doesn't work where fixnums aren't EQL
	 (t (code-char ,value))))))

(defcenum mod
  (:none #x0000)
  (:lshift #x0001)
  (:rshift #x0002)
  (:lctrl #x0040)
  (:rctrl #x0080)
  (:lalt #x0100)
  (:ralt #x0200)
  (:lmeta #x0400)
  (:rmeta #x0800)
  (:num #x1000)
  (:caps #x2000)
  (:mode #x4000)
  (:reserved #x800)
  (:ctrl #.(logior #x40 #x80))
  (:shift #.(logior #x1 #x2))
  (:alt #.(logior #x100 #x200))
  (:meta #.(logior #x400 #x800)))

(defcenum event-type
  (:noevent 0)			  ; Unused (do not remove)
  :activeevent			  ; Application loses/gains visibility
  :keydown			  ; Keys pressed
  :keyup			  ; Keys released
  :mousemotion			  ; Mouse moved
  :mousebuttondown		  ; Mouse button pressed
  :mousebuttonup		  ; Mouse button released
  :joyaxismotion		  ; Joystick axis motion
  :joyballmotion		  ; Joystick trackball motion
  :joyhatmotion			  ; Joystick hat position change
  :joybuttondown		  ; Joystick button pressed
  :joybuttonup			  ; Joystick button released
  :quit				  ; User-requested quit
  :syswmevent			  ; System specific event
  :event-reserveda		  ; Reserved for future use..
  :event-reservedb		  ; Reserved for future use..
  :videoresize			  ; User resized video mode
  :videoexpose			  ; Screen needs to be redrawn
  :event-reserved2		  ; Reserved for future use..
  :event-reserved3		  ; Reserved for future use..
  :event-reserved4		  ; Reserved for future use..
  :event-reserved5		  ; Reserved for future use..
  :event-reserved6		  ; Reserved for future use..
  :event-reserved7		  ; Reserved for future use..
  ;; Events SDL-USEREVENT through SDL-MAXEVENTS-1 are for your use
  (:userevent 24)
  (:numevents 32))

(defcstruct active-event
  (type :uint8)
  (gain :uint8)
  (state :uint8))

(defcstruct keysym
  (scancode :uint8)
  (sym key-code)
  (mod mod)
  (unicode :uint16))

(defcstruct keyboard-event
  (type :uint8)
  (which :uint8)
  (state :uint8)
  (keysym keysym))

(defcstruct motion-event
  (type :uint8)
  (which :uint8)
  (state :uint8)
  (x :uint16)
  (y :uint16)
  (xrel :int16)
  (yrel :int16))

(defcstruct button-event
  (type :uint8)
  (which :uint8)
  (button :uint8)
  (state :uint8)
  (x :uint16)
  (y :uint16))

(defcstruct joy-axis-event
  (type :uint8)
  (which :uint8)
  (axis :uint8)
  (value :int16))

(defcstruct joy-button-event
  (type :uint8)
  (which :uint8)
  (button :uint8)
  (state :uint8))


(defcunion event
  (type :uint8)
  (active active-event)
  (key keyboard-event)
  (motion motion-event)
  (button button-event)
  (jaxis joy-axis-event)
  ;;(jball joy-ball-event)
  ;;(jhat joy-hat-event)
  (jbutton joy-button-event)
  ;;(resize resize-event)
  ;;(expose expose-event) ; no content
  ;;(quit quit-event) ; no content
  ;;(user user-event)
  ;;(syswm sys-wm-event)
  )

(defcfun ("SDL_PumpEvents" pump-events) :void)
(defcfun ("SDL_PollEvent" %poll-event) :boolean (event :pointer))
(defcfun ("SDL_EnableKeyRepeat" enable-key-repeat) :boolean (delay :int) (interval :int))

(defun disable-key-repeat () (enable-key-repeat 0 0))

(defcfun ("SDL_GetKeyState" %get-key-state) :pointer (numkeys :pointer))

(defun get-key-state ()
  (with-foreign-object (n :int)
    (let ((keys (%get-key-state n)))
      (values keys (mem-ref n :int)))))

(defun key-pressed? (sym)
  (/= 0 (mem-aref (%get-key-state (null-pointer)) :uint8 (convert-to-foreign sym 'key-code))))

(defcfun ("SDL_GetModState" get-mod-state) mod)
(defcfun ("SDL_GetModState" %get-mod-state) :uint8)
(defcfun ("SDL_SetModState" set-mod-state) :void (mod mod))

(defun get-simple-mod-state ()
  (let* ((ctrl (foreign-enum-value 'mod :ctrl))
	 (alt (foreign-enum-value 'mod :alt))
	 (shift (foreign-enum-value 'mod :shift))
	 (m (logand (%get-mod-state) (logior ctrl alt shift))))
    (aref #(nil :shift :ctrl :ctrl-shift :alt :alt-shift :ctrl-alt :ctrl-alt-shift)
	  (logior (if (/= 0 (logand m shift)) 1 0)
		  (if (/= 0 (logand m ctrl)) 2 0)
		  (if (/= 0 (logand m alt)) 4 0)))))

(defun modifier? (mod) (/= 0 (logand (%get-mod-state) (foreign-enum-value 'mod mod))))

(defun poll-event ()
  (with-foreign-object (event 'event)
    (when (%poll-event event)
      (acase (event-type event)
	(:keydown (values (sym<-event event) t))
	(:keyup (values (sym<-event event) nil))
	(:mousemotion (values :mouse-motion (rel-motion<-event event)))
	(:mousebuttondown (values (mouse-button-keyword event) t))
	(:mousebuttonup (values (mouse-button-keyword event) nil))
	(:joybuttondown (values (joy-button-keyword event) t))
	(:joybuttonup (values (joy-button-keyword event) nil))
	(t it)))))

(defun mouse-button-keyword (event) (make-keyword (format nil "MOUSE-BUTTON-~D" (button<-event event))))
(defun joy-button-keyword (event) (make-keyword (format nil "JOY~D-BUTTON-~D" (which<-event event) (button<-event event))))

(defun sym<-event (event)
  (foreign-slot-value (foreign-slot-value event 'keyboard-event 'keysym) 'keysym 'sym))

(defun event-type (event)
  (foreign-enum-keyword 'event-type (foreign-slot-value event 'event 'type)))

(defun rel-motion<-event (event)
  (complex (foreign-slot-value event 'motion-event 'xrel)
	   (- (foreign-slot-value event 'motion-event 'yrel))))

(defun which<-event (event)
  (foreign-slot-value event
		      (case (event-type event)
			((:joybuttonup :joybuttondown) 'joy-button-event)
			((:joyaxismotion) 'joy-axis-event))
		      'which))

(defun button<-event (event)
  (foreign-slot-value event
		      (case (event-type event)
			((:mousebuttondown :mousebuttonup) 'button-event)
			((:joybuttonup :joybuttondown) 'joy-button-event))
		      'button))

(defcfun ("SDL_GetMouseState" %get-mouse-state) :uint8 (x :pointer) (y :pointer))

(defun get-mouse-state ()
  (with-foreign-objects ((x :int) (y :int))
    (let ((buttons (%get-mouse-state x y)))
      (values buttons (mem-ref x :int) (mem-ref y :int)))))

(defcfun ("SDL_ShowCursor" %show-cursor) :int (toggle :int))

(defun show-cursor () (%show-cursor 1))
(defun hide-cursor () (%show-cursor 0))

(defcfun ("SDL_WarpMouse" warp-mouse) :void (x :uint16) (y :uint16))

;;;; TIMER

(defcfun ("SDL_GetTicks" get-ticks) :uint32)
(defcfun ("SDL_Delay" delay) :void (ticks :uint32))