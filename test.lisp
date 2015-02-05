
(defpackage :net.cipht/sdl2-user
  (:use :cl))
(in-package :net.cipht/sdl2-user)


(defun test-display-a-blue-window-for-one-second ()
  (sdl:with-init ()
    (sdl::with-window-and-renderer (window renderer 640 480 0)
      (sdl:render-set-logical-size renderer 320 240)
      (sdl:set-render-draw-color renderer 10 10 128 255)
      (sdl:render-clear renderer)
      (sdl:render-present renderer)
      (sleep 1))))

(defun test-display-a-loaded-image ()
  (sdl:with-init ()
    (sdl-image:with-init ()
      (sdl:with-window-and-renderer (window renderer 640 480 0)
        (sdl::set-window-title window "Test")
        (let ((vtexture (sdl:create-texture renderer
                                            :pixel-format-argb8888
                                            :texture-access-streaming
                                            320 240))
              (image (sdl-image:load-texture renderer "foo.png"))
              (null-> (cffi:null-pointer)))
          (assert image)
          (sdl:render-set-logical-size renderer 320 240)
          (sdl:set-render-draw-color renderer 10 10 128 255)
          (sdl:with-event (ev)
            ;; (sdl:update-texture vtexture nil (sdl:pixels-of vbuffer) (sdl:pitch-of vbuffer))
            (sdl:render-clear renderer)
            ;; (sdl:render-copy renderer vbuffer nil nil)
            (sdl:render-copy renderer image null-> null->)
            (sdl:render-present renderer)
            (sdl:delay 1000)
            (sdl:pump-events)
            (sdl:wait-event ev)))))))
