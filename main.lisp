;;; -*- Mode: Lisp -*-

;;; Random notes:
;;; 
;;; This does not include a "switch to fullscreen" feature, because
;;; your window manager should be capable of doing this.
;;; 
;;; Use space and backspace to switch slides.

(defpackage :blitz.x11.pres
  (:use :common-lisp :xlib))
(in-package :blitz.x11.pres)

(defvar *display* nil)

(defun reshape (width height)
  (let ((aspect-ratio (/ (coerce width 'double-float)
                         (coerce (if (zerop height)
                                     1
                                     height) 
                                 'double-float))))
    (gl:viewport 0 0 width height)
    (gl:matrix-mode gl:+projection+)
    (gl:load-identity)
    (gl:ortho -1d0 1d0
              -1d0 1d0
              -1d0 1d0)
    #+ ignore
    (if (<= width height)
        (gl:ortho -10d0 10d0
                  (/ -10d0 aspect-ratio)
                  (/ 10d0 aspect-ratio)
                  1d0 -1d0)
        (gl:ortho (* -10d0 aspect-ratio)
                  (* 10d0 aspect-ratio)
                  -10d0 10d0
                  1d0 -1d0))
    (gl:matrix-mode gl:+modelview+)
    (gl:load-identity)))

(defvar *frame* 0)

(defun render-page ()
  ; (gl:push-matrix)

  (gl:color-3f 1s0 1s0 1s0)
  (gl:rect-f -0.9s0 0.9s0
             0.9s0 -0.9s0)
  
  (gl:color-3f 1s0 0s0 0s0)
  (gl:rect-f -0.5s0 0.5s0 0.5s0 -0s0)
  (gl:color-3f 0s0 0s0 1s0)
  (gl:rect-f -0.5s0 0s0 0.5s0 -0.5s0)

  ;(gl:pop-matrix)
  )

(defvar *mode* :display)
(defparameter *move-frames* 20)
(defvar *move-fps* 0)

(defun render (width height)
  (declare (ignorable width height))

  (gl:matrix-mode gl:+modelview+)
  (gl:load-identity)
  (gl:clear-color 0s0 0s0 0.1s0 1s0)
  (gl:clear gl:+color-buffer-bit+)

  #+ ignore
  (gl:rotate-f (* 20s0 (sin (coerce (/ (incf *frame*) 100) 'single-float)))
               0s0 0s0 1s0)
  (case *mode*
    (:display (render-page))
    ((:move-fwd :move-bkwd) 
     (let ((frac (/ (coerce (incf *move-fps*) 'single-float)
                    (coerce *move-frames* 'single-float))))
       (when (eq *mode* :move-bkwd)
         ;; Reverse animation
         (setq frac (- 1s0 frac)))
       ;; New page coming from above
       (gl:push-matrix)
       (gl:translate-f (- frac 1s0) 0s0 0s0) ; slightly from left
       (gl:translate-f 0s0 1s0 0s0)     ; from above
       (gl:scale-f 1s0 frac 1s0)        ; zoom effect
       (gl:translate-f 0s0 -1s0 0s0)    ; fix translation
       (render-page)
       (gl:pop-matrix)

       ;; Old page
       (gl:push-matrix)
       (gl:translate-f 0s0 -1s0 0s0)
       (gl:scale-f (- 1s0 (/ frac 1s0)) (- 1s0 frac) 1s0)
       (gl:translate-f 0s0 1s0 0s0)
       (render-page)
       (gl:pop-matrix)
       (when (>= *move-fps* *move-frames*)
         (setq *mode* :display)))))
  (glx:swap-buffers)
  )

(defun start (&key (win-width 400) (win-height 300))
  (let* ((*display* (open-default-display))
         (screen (first (display-roots *display*)))
         (root-window (screen-root screen))
         (visual (progn
                   (glx::client-info *display*)
                   (glx:choose-visual screen '(:glx-rgba :glx-double-buffer))))
         (colormap (create-colormap (glx:visual-id visual) root-window))
         (main-window (create-window :parent root-window
                                     :x 0
                                     :y 0
                                     :class :input-output
                                     :width win-width
                                     :height win-height
                                     :background (screen-black-pixel screen)
                                     :border (screen-black-pixel screen)
                                     :visual (glx:visual-id visual)
                                     :colormap colormap
                                     :event-mask (make-event-mask 
                                                  :exposure
                                                  :button-press
                                                  :key-press
                                                  :structure-notify)))
         (context (glx:create-context screen (glx:visual-id visual))))
    (unwind-protect
         (progn
           (map-window main-window)
           (glx:make-current main-window context)

           ;; Init
           (reshape win-width win-height)
           (loop
              (let ((done? nil))
                (event-case (*display* :timeout (if (eq *mode* :display) 
                                                    20
                                                    0)
                                       :force-output-p t)
                            (configure-notify (width height)
                                              (setf win-width width
                                                    win-height height)
                                              (reshape width height)
                                              t)
                            (key-press (code state)
                                       ;; XXX Use keysyms instead.
                                       (when (or (= code 22) ; backspace
                                                 (= code 65)) ; space
                                         (setq 
                                          *move-fps* 0
                                          *mode* (if (= code 22) 
                                                     :move-bkwd
                                                     :move-fwd)))
                                       t)
                            (button-press ()
                                          (setq done? t)
                                          t)
                            
                            )
                (when done?
                  (return))
                (render win-width win-height)
                )))
      (progn 
        (glx:destroy-context context)
        (destroy-window main-window)
        (close-display *display*)))))


;;; EOF
