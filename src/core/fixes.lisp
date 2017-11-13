(in-package #:parendeck2d)

;;;; Fixes for various external stuff.


;;; SDL2
(defmacro check-not-below (form lower-limit)
  (with-gensyms (rc)
    `(let ((,rc ,form))
       (when (< ,rc ,lower-limit)
         (error 'sdl2::sdl-rc-error :rc ,rc :string (sdl2-ffi.functions:sdl-get-error)))
       ,rc)))

;; SDL_GL_GetSwapInterval() can return -1 as a valid value; currently, CL-SDL restricts
;; the return value to >= 0, which is incorrect.
(defun sdl2:gl-get-swap-interval ()
  (check-not-below (sdl2-ffi.functions:sdl-gl-get-swap-interval) -1))
