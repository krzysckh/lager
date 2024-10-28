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
   (remap (car pt)  (- 0 *map-size*) *map-size* (car map-box)  (+ (car map-box)  map:sz))
   (remap (cadr pt) (- 0 *map-size*) *map-size* (cadr map-box) (+ (cadr map-box) map:sz))))

(define (remap-size-to-map size)
  (max 2 (round (remap size 0 (* 2 *map-size*) 0 map:sz))))

(define (draw-map pos size points players)
  (let* ((map-box (list (- *window-size* map:pad map:sz) map:pad map:sz map:sz))
         (user (remap-point-to-map map-box pos))
         (players (map (λ (pl) (append
                                (list (remap-size-to-map (lref pl 2)))
                                (remap-point-to-map map-box (lref pl 3)))) players)))
    (draw-rectangle map-box (make-color 255 255 255 128))
    (draw-rectangle-lines map-box 4 black)
    (for-each
     (λ (pt) (draw-circle (remap-point-to-map map-box pt) 2 green))
     points)
    (for-each
     (λ (pl) (draw-circle (cdr pl) (car pl) blue))
     players)

    (draw-circle user (remap-size-to-map size) red)
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

(define (lset* l where what)
  (if (>= where (len l))
      (append l (list what))
      (lset l where what)))

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
        ;; (print "who: " who ", m: " m)
        (tuple-case m
          ((add-player! name thread) ;; TODO: check if a player can be added
           (print "add-player!: will send data to " thread ", who=" who)
           (if (any (λ (p) (string=? (car p) name)) players)
               (begin
                 (mail who (tuple 'error "Player with that name already exists on this server"))
                 (loop players points rs))
               (begin
                 (mail who (tuple 'okay))
                 (for-each
                  (λ (pt) (mail thread (tuple 'add! pt)))
                  points)
                 (mail thread (tuple 'set-size! 10))
                 (loop (append players (list (list name thread 10))) points rs))))
          ((update-pos! pos)
           (let ((player (filter (λ (x) (equal? (lref x 1) who)) players)))
             (when (null? player)
               (print "Couldn't find " who player)
               (halt 1)) ;; TODO: don't halt, make sure this never happens

             (for-each
              (λ (p) (mail (lref p 1) (tuple 'set-pos-of! (append (car player) (list pos)))))
              (filter (λ (x) (not (equal? (car player) x))) players))

             (lets ((psz (lref (car player) 2))
                    (psz deleted (let loop ((players players) (acc 0) (deleted #n)) ;; check for collided players
                                   (let ((p (car* players)))
                                     (cond
                                      ((null? players) (values (+ psz acc) deleted))
                                      ((equal? (lref p 1) who) (loop (cdr players) acc deleted))
                                      ((> (len p) 3)
                                       (if (collision-circles? pos psz (lref p 3) (lref p 2))
                                           (cond
                                            ((> psz (lref p 2))
                                             (loop (cdr players) (+ acc (lref p 2)) (append deleted (list p))))
                                            ((< psz (lref p 2))
                                             (loop (cdr players) 0 (append deleted (list (car player)))))
                                            (else
                                             (loop (cdr players) acc deleted)))
                                           (loop (cdr players) acc deleted)))
                                      (else
                                       (loop (cdr players) acc deleted)))))))
               (for-each
                (λ (t) (for-each (λ (p) (mail (lref t 1) (tuple 'kill! p))) deleted))
                players)

               (let ((players (filter (λ (p) (not (has? (map car deleted) (car p)))) players)))
                 ;; check for collided points
                 (lets ((collided rest (let loop ((cacc #n) (racc #n) (pts points))
                                         (cond
                                          ((null? pts) (values cacc racc))
                                          ((collision-circles? (car pts) *point-radius* pos (lref (car player) 2))
                                           (mail who (tuple 'set-size! (+ (lref (car player) 2) 1)))
                                           (print who ": sized " (+ (lref (car player) 2) 1))
                                           (mail* (player-threads players) (tuple 'del! (car pts)))
                                           (loop (append cacc (list (car pts))) racc (cdr pts)))
                                          (else
                                           (loop cacc (append racc (list (car pts))) (cdr pts)))))))
                   (let L ((rs rs) (acc #n) (p collided))
                     (if (null? p)
                         (loop (if (null? collided)
                                   (edit-player players (car player) (λ (pl) (lset* pl 3 pos)))
                                   (edit-player players (car player) (λ (pl) (lset* (lset pl 2 (+ psz 1)) 3 pos))))
                               (append rest acc) rs)
                         (lets ((rs pt (random-point rs min max)))
                           (mail* (player-threads players) (tuple 'add! pt))
                           (L rs (append acc (list pt)) (cdr p))))))))))
          (else
           (print "shid, invalid-message " m)
           ;; (mail who (tuple 'invalid-message))
           (loop players points rs)))))))

(define (mesgof sym q)
  (filter (λ (x) (eq? (ref x 1) sym)) q))

(define (draw-player pos size color name)
  (draw-circle pos size color)
  (lets ((tw th (measure-text (get-font-default) name 16 0)))
    (draw-text-simple name (list (- (car pos) (/ tw 2)) (- (cadr pos) (/ th 2))) 16 white)
    (draw-text-simple name (list (- (car pos) (/ tw 2) -1) (- (cadr pos) (/ th 2) -1)) 16 black)))

;; this is a different thread because of fuckery
;; if a bot thread is started from the player thread, the thread that wants to start
;; a bot thread may not receive a confirmation (#[okay]) but rather a command that was
;; meant to be sent to a player/bot thread (e.g. #[add ...])
(define (start-player-adder)
  (thread
   'add-player
   (let loop ()
     (lets ((who v (next-mail)))
       (mail who (interact 'decider (tuple 'add-player! (ref v 1) (ref v 2))))
       (loop)))))

(define-syntax add-player
  (syntax-rules (interact)
    ((add-player name threadname)
     (interact 'add-player (tuple name threadname)))))

(define (killed? l player-name)
  (has? (map (λ (t) (lref (ref t 2) 0)) l) player-name))

(define (get-mailq)
  (let loop ((acc #n))
    (lets ((_ v (maybe-next-mail)))
      (if v
          (loop (append acc (list v)))
          acc))))

(define (update-points old-points mailq)
  (let ((del (map (C ref 2) (mesgof 'del! mailq))))
    (filter (λ (x) (not (has? del x))) (append old-points (map (C ref 2) (mesgof 'add! mailq))))))

(define (bot-find-target pos points)
  (cdar (sort (λ (a b) (< (car a) (car b))) (zip cons (map floor (map (C vec2dist pos) points)) points))))

(define (make-bot name thrid rs)
  (if (add-player name thrid)
      (thread
       thrid
       (lets ((rs shid (rand-range rs 21 37))
              (rs bx (rand-range rs 0 (* 2 *map-size*)))
              (rs by (rand-range rs 0 (* 2 *map-size*)))
              (bx (- bx *map-size*))
              (by (- by *map-size*)))
         (mail 'decider (tuple 'update-pos! (list bx by)))
         (let loop ((points #n) (x bx) (y by) (target #f))
           (let* ((mailq (get-mailq))
                  (points (update-points points mailq))
                  (target (if (or (> (len (mesgof 'set-size! mailq)) 0)
                                  (> (len (mesgof 'del! mailq)) 0)
                                  (> (len (mesgof 'add! mailq)) 0)
                                  (equal? target (list x y)))
                              #f
                              target))
                  (target (if target target (bot-find-target (list x y) points)))
                  (pos (vec2move-towards (list x y) target 15))
                  (death-note (mesgof 'kill! mailq)))
             (if (killed? death-note name)
                 #t
                 (let ()
                   (mail 'decider (tuple 'update-pos! pos))
                   (next-thread)
                   (sleep 150)
                   (loop points (car pos) (cadr pos) target)))))))
      #f))

(define (draw-leaderboard player-size player-name players)
  (let ((ps (sort (λ (a b) (> (car a) (car b)))
                  (append (map (λ (p) (list (lref p 2) (car p))) players) (list (list player-size player-name))))))
    (let loop ((i 1) (ps (take ps 10)))
      (if (or (= i 11) (null? ps))
          #t
          (begin
            (draw-text-simple
             (str i ". (" (caar ps) ") " (cadar ps))
             (list 5 (+ 5 (* i 18)))
             16
             (if (string=? (cadar ps) player-name) red black))
            (loop (+ 1 i) (cdr ps)))))))

(define (lager player-name thrname)
  (set-target-fps! 60)
  ;; TODO: figure this out as in local player mode it **will block** and decider will be slowed down
  ;; figure out = add delta time to compensate lower frame rates and don't give unfair advantage to higher frame rates

  (tuple-case (add-player player-name thrname)
    ((error why)
     (error "couldn't add player to the game: " why))
    (else
     #t))

  (thread (let loop ((i 0))
            (if (= i 10)
                #t
                (let ((name (string-append "local-bot@" (number->string i))))
                  (make-bot name (string->symbol name) (seed->rands (time-ns)))
                  (sleep 10)
                  (loop (+ 1 i))))))

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
     (lets ((mailq (get-mailq))
            (mp (map (λ (v) (remap v 0 *window-size* -1 1)) (mouse-pos)))
            (x y (values
                  (+ x (* speed-mult *speed-base* (car mp)))
                  (+ y (* speed-mult *speed-base* (cadr mp)))))
            (pos (list x y))
            (camera (list *cam-offset* pos 0 zoom))
            ;; update data based on mailq
            (points (update-points points mailq))
            (size (let ((msgs (mesgof 'set-size! mailq)))
                    (if (null? msgs)
                        size
                        (ref (last msgs 'bug) 2))))
            (death-note (mesgof 'kill! mailq))
            (dnames (map car players))
            (players (filter (λ (p) (not (killed? death-note (car p)))) players))
            (players (let loop ((msgs (mesgof 'set-pos-of! mailq)) (players players))
                       (cond
                        ((null? msgs) players)
                        ((assoc (car (ref (car msgs) 2)) players) ;; player exists in mem, change data
                         (loop (cdr msgs) (edit-player players (ref (car msgs) 2) (λ (pl) (ref (car msgs) 2)))))
                        (else
                         (loop (cdr msgs) (append players (list (ref (car msgs) 2))))))))
            )

       (when (not (null? death-note))
         (print "death-note: " death-note))

       (when (killed? death-note player-name)
         (print 'died)
         (halt 0))

       ;; ask the decider to update location every n frames or if i think a a point was touched
       (when (or (any (λ (pt) (collision-circles? pt *point-radius* pos size)) points) (= frame-ctr 0))
         (mail 'decider (tuple 'update-pos! pos)))

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
        (draw-map pos size points players)
        (draw-leaderboard size player-name players)
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
  (start-player-adder)
  (thread 'threadmain (lager "local player" 'threadmain))
  (ref (wait-mail) 2))
