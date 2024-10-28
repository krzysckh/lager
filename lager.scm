(import
 (owl toplevel)
 (raylib)
 (lager const)
 (lager decider)
 (lager common)
 )

(define (draw-grid)
  (for-each
   (λ (v)
     (draw-line-simple dgl:min v dgl:max v white)
     (draw-line-simple v dgl:min v dgl:max white))
   (iota dgl:min *grid-size* dgl:max))) ;; assuming width == height

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
