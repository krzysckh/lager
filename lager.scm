(import
 (owl toplevel)
 (prefix (owl sys) sys/)
 (raylib)
 (lager const)
 (lager decider)
 (lager common)
 (lager assets)
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
  (lets ((font (asset 'font))
         (tw th (measure-text font name 16 0)))
    (draw-text font name (list (- (car pos) (/ tw 2)) (- (cadr pos) (/ th 2))) 16 0 black)))

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
                  (append (map (λ (p) (list (lref p 2) (car p))) players) (list (list player-size player-name)))))
        (font (asset 'font)))
    (let loop ((i 1) (ps (take ps 10)))
      (if (or (= i 11) (null? ps))
          #t
          (begin
            (draw-text
             font
             (str i ". (" (caar ps) ") " (cadar ps))
             (list 5 (+ 5 (* i 18)))
             16
             0
             (if (string=? (cadar ps) player-name) red black))
            (loop (+ 1 i) (cdr ps)))))))

(define (lager player-name thrname)
  ;; TODO: figure this out as in local player mode it **will block** and decider will be slowed down
  ;; figure out = add delta time to compensate lower frame rates and don't give unfair advantage to higher frame rates

  ;; (tuple-case (add-player player-name thrname)
  ;;   ((error why)
  ;;    (error "couldn't add player to the game: " why))
  ;;   (else
  ;;    #t))

  (set-target-fps! 60)
  (mail 'decider (tuple 'add-player! player-name thrname))

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
           (players (let loop ((msgs (mesgof 'set-pos-of! mailq)) (players players))
                      (cond
                       ((null? msgs) players)
                       ((assoc (car (ref (car msgs) 2)) players) ;; player exists in mem, change data
                        (loop (cdr msgs) (edit-player players (ref (car msgs) 2) (λ (pl) (ref (car msgs) 2)))))
                       (else
                        (loop (cdr msgs) (append players (list (ref (car msgs) 2))))))))
           (players (filter (λ (p) (not (killed? death-note (car p)))) players))
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
          (die)
          (loop
           x
           y
           points
           size
           (+ zoom (* 0.01 (mouse-wheel)))
           speed-mult
           players
           (modulo (+ frame-ctr 1) *frames-per-pos*))))))

(define (connect-to-decider player-thread ip)
  (let* ((con (open-connection (sys/resolve-host ip) *port*))
         (bs (port->byte-stream con)))
    (print "[networked decider] con: " con)
    (thread
     (let loop ()
       (when-readable con)
       (let* ((size (u16->n (bytevector->list (try-get-block con 2 #f))))
              (b (bytevector->list (try-get-block con size #t)))
              (v (reintern (fasl-decode b (tuple 'bad)))))
         (mail player-thread v)
         (loop))))

    (let loop ()
      (lets ((who v (next-mail)))
        (let ((fasl (fasl-encode v)))
          (write-bytes con (n->u16 (len fasl)))
          (write-bytes con fasl)
          (loop))))))

(define (start-online-lager srv uname)
  (thread 'decider (connect-to-decider 'threadmain srv))
  (thread 'threadmain (lager uname 'threadmain))
  (lets ((_ v (next-mail)))
    v))

(define (start-offline-lager srv uname)
  (start-player-adder)
  (thread 'decider (decider))
  (thread 'threadmain (lager uname 'threadmain))
  (let loop ((i 0))
    (if (= i 10)
        #t
        (let ((uname (string-append "local-bot@" (number->string i))))
          (make-bot uname (string->symbol uname) (seed->rands (time-ns)))
          (sleep 10)
          (loop (+ 1 i)))))
  (lets ((_ v (next-mail)))
    v))

(define (input-box box picked? text)
  (let ((font (asset 'font24)))
    (draw-rectangle box gray)
    (draw-rectangle-lines box 3 (if picked? red black))
    (draw-text font text (list (+ 3 (car box)) (+ (cadr box) 6) (caddr box) (cadddr box)) 24 0 black)))

(define (but-last l)
  (reverse (cdr* (reverse l)))) ;; TODO: slow

(define (update-text l)
  (let ((updated (let loop ((acc #n))
                   (let ((c (char-pressed)))
                     (if (= c 0)
                         (append l acc)
                         (loop (append acc (list c))))))))
    (if (key-down? key-backspace)
        (but-last updated)
        updated)))

(define (menu)
  (set-target-fps! 15)
  (let ((font64 (asset 'font64))
        (font32 (asset 'font32))
        (font24 (asset 'font24))
        (font (asset 'font)))
    (let loop ((picked 'srv) (srv (string->list "pub.krzysckh.org")) (uname (string->list "local-player")))
      (let* ((srv-box     (list (/ *window-size* 4) (* 3 (/ *window-size* 8)) (/ *window-size* 2) 32))
             (uname-box   (list (/ *window-size* 4) (* 4 (/ *window-size* 8)) (/ *window-size* 2) 32))
             (start-box   (list (/ *window-size* 4) (* 5 (/ *window-size* 8)) (/ *window-size* 2) 64))
             (offline-box (list (/ *window-size* 4) (* 6 (/ *window-size* 8)) (/ *window-size* 2) 48))
             (picked (cond
                      ((collision-point-rect? (mouse-pos) srv-box) 'srv)
                      ((collision-point-rect? (mouse-pos) uname-box) 'uname)
                      ((collision-point-rect? (mouse-pos) start-box) 'start)
                      ((collision-point-rect? (mouse-pos) offline-box) 'offline)
                      (else picked)))
             (srv (if (eq? picked 'srv) (update-text srv) srv))
             (uname (if (eq? picked 'uname) (update-text uname) uname))
             (bpressed? (mouse-btn-pressed? mouse-button-left))
             )
        (draw ;; rawdogging the gui
         (clear-background gray)
         (draw-grid)
         (lets ((t "(lager)") (tw th (measure-text font64 t 64 0)))
           (draw-text font64 t (list (- (/ *window-size* 2) (/ tw 2) -10) 90) 64 0 black)
           (draw-text font64 t (list (- (/ *window-size* 2) (/ tw 2))     80) 64 0 white))

         (input-box srv-box (eq? 'srv picked) (list->string srv))
         (input-box uname-box (eq? 'uname picked) (list->string uname))

         (draw-rectangle start-box gray)
         (draw-rectangle-lines start-box 4 (if (eq? picked 'start) red black))

         (lets ((t "(join)") (tw th (measure-text font32 t 32 0)))
           (draw-text font32 t (list (- (/ *window-size* 2) (/ tw 2) -5) (+ (cadr start-box) 19)) 32 0 black)
           (draw-text font32 t (list (- (/ *window-size* 2) (/ tw 2)) (+ (cadr start-box) 16)) 32 0 white))

         (draw-rectangle offline-box gray)
         (draw-rectangle-lines offline-box 4 (if (eq? picked 'offline) red black))

         (lets ((t "(play-offline)") (tw th (measure-text font24 t 24 0)))
           (draw-text font24 t (list (- (/ *window-size* 2) (/ tw 2) -5) (+ (cadr offline-box) 17)) 24 0 black)
           (draw-text font24 t (list (- (/ *window-size* 2) (/ tw 2)) (+ (cadr offline-box) 12)) 24 0 white))
         )

        (cond
         ((window-should-close?) (die))
         ((and bpressed? (eq? picked 'start)) (start-online-lager (list->string srv) (list->string uname)))
         ((and bpressed? (eq? picked 'offline)) (start-offline-lager (list->string srv) (list->string uname)))
         (else
          (loop picked srv uname)))))))

(lambda (args)
  (with-window
   *window-size* *window-size* "*lager*"
   (let ()
     (set-exit-key! 0)
     (start-asset-handler)
     (initialize-default-assets)
     (menu)
     (wait-mail))))
