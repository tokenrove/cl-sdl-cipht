;;;; VIDEO

(in-package :net.cipht/sdl2)

;;; PIXEL FORMATS ETC

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

(defcfun ("SDL_MapRGBA" map-rgba) :uint32
  (format (:pointer (:struct pixel-format)))
  (r :uint8) (g :uint8) (b :uint8) (a :uint8))

;;; SURFACE

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

(defcfun ("SDL_ConvertSurface" convert-surface)
    (:pointer (:struct surface))
  (source (:pointer (:struct surface)))
  (format (:pointer (:struct pixel-format)))
  (flags surface-flags))

(defcfun ("SDL_SetSurfacePalette" set-surface-palette)
    :int
  (surface (:pointer (:struct surface)))
  (palette (:pointer (:struct palette))))

;;; XXX these should all be methods, really

(defun format-of (surface)
  (foreign-slot-value surface '(:struct surface) 'format))

(defun palette-of (surface)
  (with-foreign-slots ((palette) (format-of surface) (:struct pixel-format))
    (nil<-null palette)))

(defun width-of (surface)
  (foreign-slot-value surface '(:struct surface) 'w))
(defun height-of (surface)
  (foreign-slot-value surface '(:struct surface) 'h))
(defun pitch-of (surface)
  (foreign-slot-value surface '(:struct surface) 'pitch))
(defun pixels-of (surface)
  (foreign-slot-value surface '(:struct surface) 'pixels))

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
          (8 (list :uint8 pitch))
          (16 (list :uint16 (ash pitch -1)))
          (32 (list :uint32 (ash pitch -2))))
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

;;; WINDOW

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

(defctype window :pointer)

(defcfun ("SDL_DestroyWindow" destroy-window) :void (window window))
(defcfun ("SDL_GetWindowID" get-window-id) :uint32 (window window))

(defcfun ("SDL_SetWindowTitle" set-window-title)
    :void
  (window :pointer)
  (title :string))

(defcfun ("SDL_GetWindowTitle" get-window-title)
    :string
  (window :pointer))

;;; RENDERER

(defcenum renderer-flags
  (:renderer-software #x00000001) ; The renderer is a software fallback
  (:renderer-accelerated #x00000002) ; The renderer uses hardware acceleration
  (:renderer-presentvsync #x00000004) ; Present is synchronized with the refresh rate
  (:renderer-targettexture #x00000008)) ; The renderer supports rendering to texture

(defctype renderer :pointer)
(defctype texture :pointer)

(defcfun ("SDL_CreateWindowAndRenderer" create-window-and-renderer)
    :int
  (width :int)
  (height :int)
  (window-flags window-flags)
  (window (:pointer window))
  (renderer (:pointer renderer)))

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

;;; TEXTURE

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
