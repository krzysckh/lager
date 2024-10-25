(import
 (owl toplevel)
 (raylib))

(define *window-width* 800)
(define *window-height* 800)

(define *cam-offset* (list (/ *window-width* 2) (/ *window-height* 2)))

(define *grid-size* 50)

(define *speed-base* 2)
(define *map-mult* 2)
(define *map-size* (list (* *window-width* 2) (* *window-height* 2)))

(define dgl:min (- 0 (car *map-size*)))
(define dgl:max (car *map-size*))
(define (draw-grid)
  (for-each
   (λ (v)
     (draw-line-simple dgl:min v dgl:max v white)
     (draw-line-simple v dgl:min v dgl:max white))
   (iota dgl:min *grid-size* dgl:max))) ;; assuming width == height

;; in camera coords
(define map:pad 20)
(define map:sz 150)
(define (draw-map pos)
  (let* ((map-box (list (- *window-width* map:pad map:sz) map:pad map:sz map:sz))
         (pt (list
              (remap (car pos)  (- 0 (car *map-size*))  (car *map-size*)  (car map-box)  (+ (car map-box) map:sz))
              (remap (cadr pos) (- 0 (cadr *map-size*)) (cadr *map-size*) (cadr map-box) (+ (cadr map-box) map:sz)))))
    (print "pos: " pos)
    (print "pt: " pt)
    (draw-rectangle map-box (make-color 255 255 255 64))
    (draw-rectangle-lines map-box 4 black)
    (draw-circle pt 10 red)
    ))

(λ (_)
  (set-target-fps! 60)
  (with-window
   *window-width* *window-height* "*lager*"
   (let loop ((x 0)
              (y 0)
              (zoom 1.0)
              (speed-mult 2.0)
              )
     (let* ((x (if (key-down? key-a) (- x (* speed-mult *speed-base*)) x))
            (x (if (key-down? key-d) (+ x (* speed-mult *speed-base*)) x))
            (y (if (key-down? key-w) (- y (* speed-mult *speed-base*)) y))
            (y (if (key-down? key-s) (+ y (* speed-mult *speed-base*)) y))
            (pos (list x y))
            (camera (list *cam-offset* pos 0 zoom)))
       (draw
        (clear-background gray)
        (with-camera2d
         camera
         (begin
           (draw-grid)
           (draw-circle pos 10 red)
           ))
        (draw-map pos)
        (draw-fps 0 0)
        )
       (if (window-should-close?)
           0
           (loop x y (+ zoom (mouse-wheel)) speed-mult))))))
