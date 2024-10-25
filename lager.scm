(import
 (owl toplevel)
 (raylib))

(define *window-width* 600)
(define *window-height* 600)

(define *cam-offset* (list (/ *window-width* 2) (/ *window-height* 2)))

(define *grid-size* 50)
(define *point-radius* 10)

(define *speed-base* 2)
(define *map-mult* 2)
(define *map-size* (list (* *window-width* 2) (* *window-height* 2)))
(define *n-points* 128)

(define dgl:min (- 0 (car *map-size*)))
(define dgl:max (car *map-size*))
(define (draw-grid)
  (for-each
   (λ (v)
     (draw-line-simple dgl:min v dgl:max v white)
     (draw-line-simple v dgl:min v dgl:max white))
   (iota dgl:min *grid-size* dgl:max))) ;; assuming width == height


(define (maybe-next-mail)
  (let ((envelope (check-mail)))
    (if (tuple? envelope)
        (values (ref envelope 1) (ref envelope 2))
        (values #f #f))))

;; in camera coords
(define map:pad 20)
(define map:sz 150)

(define (remap-point-to-map map-box pt)
  (list
   (remap (car pt)  (- 0 (car *map-size*))  (car *map-size*)  (car map-box)  (+ (car map-box) map:sz))
   (remap (cadr pt) (- 0 (cadr *map-size*)) (cadr *map-size*) (cadr map-box) (+ (cadr map-box) map:sz))))

(define (draw-map pos points)
  (let* ((map-box (list (- *window-width* map:pad map:sz) map:pad map:sz map:sz))
         (user (remap-point-to-map map-box pos)))
    (draw-rectangle map-box (make-color 255 255 255 128))
    (draw-rectangle-lines map-box 4 black)
    (for-each
     (λ (pt) (draw-circle (remap-point-to-map map-box pt) 2 green))
     points)
    (draw-circle user 5 red)
    ))

;; rs min max → rs (x y)
(define (random-point rs min max)
  (let* ((rs x (rand-range rs min max))
         (rs y (rand-range rs min max)))
    (values rs (list x y))))

;; generate n points, call cb with every
(define (random-points rs n min max cb)
  (if (= n 0)
      rs
      (lets ((rs pt (random-point rs min max)))
        (cb pt)
        (random-points rs (- n 1) min max cb))))

(define (pointer)
  (lets ((min (- 0 (car *map-size*)))
         (max (cadr *map-size*))
         (rs (random-points
              (seed->rands (time-ms))
              *n-points* min max
              (λ (x) (mail 'threadmain (tuple 'add! x))))))
    (let loop ((rs rs))
      (lets ((who m (next-mail)))
        (print "who: " who ", m: " m)
        (tuple-case m
          ((delete! ref)
           (lets ((rs pt (random-point rs min max)))
             (print "Adding point: " pt)
             (mail who (tuple 'add! pt))
             (loop rs)))
          (else
           (print "shid, invalid-message " m)
           (mail who (tuple 'invalid-message))
           (loop rs)))))))

(define (main)
  (thread 'pointer (pointer))
  (next-thread)
  (set-target-fps! 60)
  (with-window
   *window-width* *window-height* "*lager*"
   (let loop ((x 0)
              (y 0)
              (points #n)
              (size 0)
              (zoom 1.0)
              (speed-mult 2.0)
              )
     (let* ((mailq (let loop ((acc #n))
                     (lets ((_ v (maybe-next-mail)))
                       (if v
                           (loop (append acc (list v)))
                           acc))))
            (x (if (key-down? key-a) (- x (* speed-mult *speed-base*)) x))
            (x (if (key-down? key-d) (+ x (* speed-mult *speed-base*)) x))
            (y (if (key-down? key-w) (- y (* speed-mult *speed-base*)) y))
            (y (if (key-down? key-s) (+ y (* speed-mult *speed-base*)) y))
            (pos (list x y))
            (camera (list *cam-offset* pos 0 zoom))
            ;; update data based on mailq
            (points (append points (map (C ref 2) (filter (λ (x) (eq? (ref x 1) 'add!)) mailq))))
            (points size (let loop ((+size 0) (acc #n) (points points) (i 0))
                           (cond
                            ((null? points) (values acc (+ size +size)))
                            ((collision-circles? (car points) *point-radius* pos (+ 10 size))
                             (mail 'pointer (tuple 'delete! i))
                             (loop (+ +size 1) acc (cdr points) (+ i 1)))
                            (else
                             (loop +size (append acc (list (car points))) (cdr points) (+ i 1)))))))
       (draw
        (clear-background gray)
        (with-camera2d
         camera
         (begin
           (draw-grid)
           (for-each (λ (pos) (draw-circle pos *point-radius* green)) points)
           (draw-circle pos (+ 10 size) red)
           ))
        (draw-map pos points)
        (draw-fps 0 0)
        )
       (if (window-should-close?)
           0
           (loop x y points size (+ zoom (* 0.2 (mouse-wheel))) speed-mult))))))

(λ (_)
  (thread 'threadmain (main)))
