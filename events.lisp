(in-package :net.cipht/sdl2)

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
  (scancode :int)
  ;; (sym key-code)
  (sym :int32)
  (mod :uint16)
  (unicode :uint32))

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
  (let ((sv (foreign-slot-value event '(:union event) 'type)))
   (foreign-enum-keyword 'event-type sv :errorp nil)))

(defun event-keysym (event)
  (getf (foreign-slot-value event '(:struct keyboard-event) 'keysym) 'sym))

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
