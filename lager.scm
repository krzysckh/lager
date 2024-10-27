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
    (draw-circle user (max 2 (round (remap size 0 (* 2 *map-size*) 0 map:sz))) red)
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
  (if (null? targets)
      #t
      (begin
        (mail (car targets) v)
        (mail* (cdr targets) v))))

(define (player-threads l)
  (map cadr l))

(define (edit-player players player f)
  (let loop ((acc #n) (players players))
    (cond
     ((null? players) acc)
     ((string=? (car player) (caar players))
      (loop (append acc (list (f (car players)))) (cdr players)))
     (else
      (loop (append acc (list (car players))) (cdr players))))))

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
           (mail thread (tuple 'set-size! 10))
           (loop (append players (list (list name thread 10))) points rs))
          ((update-pos! pos)
           (let ((player (filter (λ (x) (equal? (lref x 1) who)) players)))
             (when (null? player)
               (print "Couldn't find " who player)
               (halt 1)) ;; TODO: don't halt, make sure this never happens

             (for-each (λ (p) (mail (lref p 1) (tuple 'set-pos-of! (append (car player) (list pos)))))
                       (filter (λ (x) (not (equal? (car player) x))) players))

             (lets ((collided rest (let loop ((cacc #n) (racc #n) (pts points))
                                     (cond
                                      ((null? pts) (values cacc racc))
                                      ((collision-circles? (car pts) *point-radius* pos (lref (car player) 2))
                                       (mail who (tuple 'set-size! (+ (lref (car player) 2) 1)))
                                       (mail* (player-threads players) (tuple 'del! (car pts)))
                                       (loop (append cacc (list (car pts))) racc (cdr pts)))
                                      (else
                                       (loop cacc (append racc (list (car pts))) (cdr pts)))))))
               (let L ((rs rs) (acc #n) (p collided))
                 (if (null? p)
                     (loop (if (null? collided)
                               players
                               (edit-player players (car player) (λ (pl) (lset pl 2 (+ (lref pl 2) 1)))))
                           (append rest acc) rs)
                     (lets ((rs pt (random-point rs min max)))
                       (mail* (player-threads players) (tuple 'add! pt))
                       (L rs (append acc (list pt)) (cdr p))))))))
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

(define (draw-player pos size color name)
  (draw-circle pos size color)
  (lets ((tw th (measure-text (get-font-default) name 16 0)))
    (draw-text-simple name (list (- (car pos) (/ tw 2)) (- (cadr pos) (/ th 2))) 16 white)
    (draw-text-simple name (list (- (car pos) (/ tw 2) -1) (- (cadr pos) (/ th 2) -1)) 16 black)))

(define (lager player-name thrname)
  (set-target-fps! 60)
  (when (not (add-player player-name thrname))
    (error "shid" ""))

  (add-player "bot test" 'bots)
  (thread
   'bots
   (let ()
     (mail 'decider (tuple 'update-pos! (list 0 0)))
     (wait-mail)
     (let loop ()
       (print "bots: skipping: " (check-mail))
       (if (check-mail)
           (loop)
           #t))
     (let loop ((x 0))
       (lets ((a b (next-mail)))
         (print "[bot sigma] " a ": " b)
         (mail 'decider (tuple 'update-pos! (list x 0)))
         (next-thread)
         (print "bots after next-thread")
         (loop (+ x 3))))))

  (with-window
   *window-size* *window-size* "*lager*"
   (let loop ((x 0)
              (y 0)
              (points #n)
              (size 0)
              (zoom 1.0)
              (speed-mult 2.0)
              (players ())
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
            (size (let ((msgs (mesgof 'set-size! mailq)))
                    (if (null? msgs)
                        size
                        (ref (last msgs 'bug) 2))))
            (players (let loop ((msgs (mesgof 'set-pos-of! mailq)) (players players))
                       (cond
                        ((null? msgs) players)
                        ((assoc (car (ref (car msgs) 2)) players) ;; player exists in mem, change data
                         (loop (cdr msgs) (edit-player players (ref (car msgs) 2) (λ (pl) (ref (car msgs) 2)))))
                        (else
                         (loop (cdr msgs) (append players (list (ref (car msgs) 2))))))))
            )

       ;; ask the decider to update location every n frames or if i think a a point was touched
       (when (or (any (λ (pt) (collision-circles? pt *point-radius* pos size)) points) (= frame-ctr 0))
         (mail 'decider (tuple 'update-pos! pos)))

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
           (draw-player pos size red player-name)
           (for-each (λ (pl) (draw-player (lref pl 3) (lref pl 2) blue (car pl))) players)
           ))
        (draw-map pos points size)
        (draw-fps 0 0)
        )
       (if (window-should-close?)
           0
           (loop
            x
            y
            points
            size
            (+ zoom (* 0.01 (mouse-wheel)))
            speed-mult
            players
            (modulo (+ frame-ctr 1) *frames-per-pos*)))))))

(λ (_)
  (thread 'decider (decider))
  (thread 'threadmain (lager "local player" 'threadmain))
  (ref (wait-mail) 2))
