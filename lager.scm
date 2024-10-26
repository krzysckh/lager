(import
 (owl toplevel)
 (raylib))

(define *window-size* 600)

(define *cam-offset* (list (/ *window-size* 2) (/ *window-size* 2)))

(define *grid-size* 50)
(define *point-radius* 10)
(define *frames-per-pos* 8)

(define *speed-base* 5)
(define *map-mult* 2)
(define *map-size* (* *window-size* 4))

(define *n-points* 128)

(define dgl:min (- 0 *map-size*))
(define dgl:max *map-size*)
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
   (remap (car pt)  (- 0 *map-size*)  *map-size*  (car map-box)  (+ (car map-box) map:sz))
   (remap (cadr pt) (- 0 *map-size*) *map-size* (cadr map-box) (+ (cadr map-box) map:sz))))

(define (draw-map pos points size)
  (let* ((map-box (list (- *window-size* map:pad map:sz) map:pad map:sz map:sz))
         (user (remap-point-to-map map-box pos)))
    (draw-rectangle map-box (make-color 255 255 255 128))
    (draw-rectangle-lines map-box 4 black)
    (for-each
     (λ (pt) (draw-circle (remap-point-to-map map-box pt) 2 green))
     points)
    (draw-circle user (max 1 (round (remap size 0 (* 2 *map-size*) 0 map:sz))) red)
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

(define (mail* targets v)
  (print "mail* mailing to: " targets)
  (if (null? targets)
      #t
      (begin
        (mail (car targets) v)
        (mail* (cdr targets) v))))

(define (player-threads l)
  (map cadr l))

;; meant to be ran over the network
(define (decider)
  (lets ((min (- 0 *map-size*))
         (max *map-size*)
         (points rs (random-points
                     (seed->rands (time-ms))
                     *n-points* min max
                     (λ (x) (print "init-point " x))
                     ())))

    (print "decider ready")
    (let loop ((players #n) (points points) (rs rs))
      (lets ((who m (next-mail)))
        (print "who: " who ", m: " m)
        (tuple-case m
          ((add-player! name thread) ;; TODO: check if a player can be added
           (mail who (tuple 'okay))
           (for-each
            (λ (pt) (mail thread (tuple 'add! pt)))
            points)
           (loop (append players (list (list name thread 0))) points rs))
          ((update-pos! pos size) ;; TODO: store size per user somewhere here
           (lets ((collided rest (let loop ((cacc #n) (racc #n) (pts points))
                                   (cond
                                    ((null? pts) (values cacc racc))
                                    ((collision-circles? (car pts) *point-radius* pos (+ 10 size))
                                     (mail who (tuple 'enlarge!))
                                     (mail* (player-threads players) (tuple 'del! (car pts)))
                                     (loop (append cacc (list (car pts))) racc (cdr pts)))
                                    (else
                                     (loop cacc (append racc (list (car pts))) (cdr pts)))))))
             (let L ((rs rs) (acc #n) (p collided))
               (if (null? p)
                   (loop players (append rest acc) rs)
                   (lets ((rs pt (random-point rs min max)))
                     (mail* (player-threads players) (tuple 'add! pt))
                     (L rs (append acc (list pt)) (cdr p)))))))
          (else
           (print "shid, invalid-message " m)
           ;; (mail who (tuple 'invalid-message))
           (loop players points rs)))))))

(define (mesgof sym q)
  (filter (λ (x) (eq? (ref x 1) sym)) q))

(define (add-player name thread)
  (let ((v (interact 'decider (tuple 'add-player! name thread))))
    (tuple-case v
      ((error why)
       (print-to stderr "couldn't add-player " name ": " why)
       #f)
      (else ;; okay we ballin
       (print "okay we ballin")
       #t))))

(define (lager player-name thrname)
  (set-target-fps! 60)
  (when (not (add-player player-name thrname))
    (error "shid" ""))

  (with-window
   *window-size* *window-size* "*lager*"
   (let loop ((x 0)
              (y 0)
              (points #n)
              (size 0)
              (zoom 1.0)
              (speed-mult 2.0)
              (frame-ctr 0)
              )
     (lets ((mailq (let loop ((acc #n))
                     (lets ((_ v (maybe-next-mail)))
                       (if v
                           (loop (append acc (list v)))
                           acc))))
            ;; (x (if (key-down? key-a) (- x (* speed-mult *speed-base*)) x))
            ;; (x (if (key-down? key-d) (+ x (* speed-mult *speed-base*)) x))
            ;; (y (if (key-down? key-w) (- y (* speed-mult *speed-base*)) y))
            ;; (y (if (key-down? key-s) (+ y (* speed-mult *speed-base*)) y))
            (mp (map (λ (v) (remap v 0 *window-size* -1 1)) (mouse-pos)))
            (x y (values
                  (+ x (* speed-mult *speed-base* (car mp)))
                  (+ y (* speed-mult *speed-base* (cadr mp)))))
            (pos (list x y))
            (camera (list *cam-offset* pos 0 zoom))
            ;; update data based on mailq
            (points (fold (λ (acc pt) (filter (λ (x) (not (equal? x pt))) acc)) points (map (C ref 2) (mesgof 'del! mailq))))
            (points (append points (map (C ref 2) (mesgof 'add! mailq))))
            (size (+ size (len (mesgof 'enlarge! mailq))))
            )

       ;; ask the decider to update location every n frames or if i think a a point was touched
       (when (or (any (λ (pt) (collision-circles? pt *point-radius* pos (+ 10 size))) points) (= frame-ctr 0))
         (mail 'decider (tuple 'update-pos! pos size)))

       ;; (when (key-down? key-equal)
       ;;   (let loop ((i 0))
       ;;     (if (= i 10)
       ;;         #n
       ;;         (let ()
       ;;           (print (mail 'threadmain (tuple 'enlarge!)))
       ;;           (loop (+ 1 i))))))

       (draw
        (clear-background gray)
        (with-camera2d
         camera
         (begin
           (draw-grid)
           (for-each (λ (pos) (draw-circle pos *point-radius* green)) points)
           (draw-circle pos (+ 10 size) red)
           (lets ((tw th (measure-text (get-font-default) player-name 14 0)))
             (draw-text-simple player-name (list (- (car pos) (/ tw 2)) (- (cadr pos) (/ th 2))) 14 white)
             (draw-text-simple player-name (list (- (car pos) (/ tw 2) -1) (- (cadr pos) (/ th 2) -1)) 14 black))
           ))
        (draw-map pos points size)
        (draw-fps 0 0)
        )
       (if (window-should-close?)
           0
           (loop x y points size (+ zoom (* 0.01 (mouse-wheel))) speed-mult (modulo (+ frame-ctr 1) *frames-per-pos*)))))))

(λ (_)
  (thread 'decider (decider))
  (thread 'threadmain (lager "local player" 'threadmain))
  (ref (wait-mail) 2))
