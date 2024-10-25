(import
 (owl toplevel)
 (raylib))

(define *window-width* 600)
(define *window-height* 600)

(define *cam-offset* (list (/ *window-width* 2) (/ *window-height* 2)))

(define *grid-size* 50)
(define *point-radius* 10)
(define *frames-per-pos* 5)

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
(define (random-points rs n min max cb acc)
  (if (= n 0)
      (values acc rs)
      (lets ((rs pt (random-point rs min max)))
        (cb pt)
        (random-points rs (- n 1) min max cb (append acc (list pt))))))

;; meant to be ran over the network
(define (decider)
  (lets ((min (- 0 (car *map-size*)))
         (max (cadr *map-size*))
         (points rs (random-points
                     (seed->rands (time-ms))
                     *n-points* min max
                     (λ (x) (mail 'threadmain (tuple 'add! x)))
                     ())))

    (print "decider ready")
    (let loop ((points points) (rs rs))
      (lets ((who m (next-mail)))
        (print "who: " who ", m: " m)
        (tuple-case m
          ((update-pos! pos size) ;; TODO: store size per user somewhere here
           (lets ((collided rest (let loop ((cacc #n) (racc #n) (pts points))
                                   (cond
                                    ((null? pts) (values cacc racc))
                                    ((collision-circles? (car pts) *point-radius* pos (+ 10 size))
                                     (mail who (tuple 'enlarge!))
                                     (mail who (tuple 'del! (car pts)))
                                     (loop (append cacc (list (car pts))) racc (cdr pts)))
                                    (else
                                     (loop cacc (append racc (list (car pts))) (cdr pts)))))))
             (let L ((rs rs) (acc #n) (p collided))
               (if (null? p)
                   (loop (append rest acc) rs)
                   (lets ((rs pt (random-point rs min max)))
                     (mail who (tuple 'add! pt))
                     (L rs (append acc (list pt)) (cdr p)))))))
          (else
           (print "shid, invalid-message " m)
           ;; (mail who (tuple 'invalid-message))
           (loop points rs)))))))

(define (mesgof sym q)
  (filter (λ (x) (eq? (ref x 1) sym)) q))

(define (main)
  (thread 'decider (decider))
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
              (frame-ctr 0)
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
            (points (fold (λ (acc pt) (filter (λ (x) (not (equal? x pt))) acc)) points (map (C ref 2) (mesgof 'del! mailq))))
            (points (append points (map (C ref 2) (mesgof 'add! mailq))))
            (size (+ size (len (mesgof 'enlarge! mailq))))
            )

       (when (= frame-ctr 0)
         (mail 'decider (tuple 'update-pos! pos size)))

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
           (loop x y points size (+ zoom (* 0.2 (mouse-wheel))) speed-mult (modulo (+ frame-ctr 1) *frames-per-pos*)))))))

(λ (_)
  (thread 'threadmain (main))
  (ref (wait-mail) 2))
