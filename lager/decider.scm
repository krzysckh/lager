(define-library (lager decider)
  (import
   (owl toplevel)
   (raylib)
   (lager const)
   (lager common)
   )

  (export
   decider
   )

  (begin
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
                 (if (null? player)
                     (let ()
                       (print "[maybe-error] Couldn't find " who player)
                       (loop players points rs))
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
                        (λ (p) (mail (lref p 1) (tuple 'set-pos-of! (append (car player) (list pos)))))
                        (filter (λ (x) (not (equal? (car player) x))) players))

                       (for-each
                        (λ (t) (for-each (λ (p) (mail (lref t 1) (tuple 'kill! p))) deleted))
                        players)

                       (let ((players (filter (λ (p) (not (has? (map car deleted) (car p)))) players)))
                         ;; check for collided points
                         (lets ((collided rest (let loop ((cacc #n) (racc #n) (pts points))
                                                 (cond
                                                  ((null? pts) (values cacc racc))
                                                  ((collision-circles? (car pts) *point-radius* pos (lref (car player) 2))
                                                   (mail* (player-threads players) (tuple 'del! (car pts)))
                                                   (loop (append cacc (list (car pts))) racc (cdr pts)))
                                                  (else
                                                   (loop cacc (append racc (list (car pts))) (cdr pts)))))))
                           (mail who (tuple 'set-size! (+ psz (len collided))))
                           (let L ((rs rs) (acc #n) (p collided))
                             (if (null? p)
                                 (loop (edit-player players (car player) (λ (pl) (lset* (lset pl 2 (+ psz (len collided))) 3 pos))) (append rest acc) rs)
                                 (lets ((rs pt (random-point rs min max)))
                                   (mail* (player-threads players) (tuple 'add! pt))
                                   (L rs (append acc (list pt)) (cdr p)))))))))))
              (else
               (print "shid, invalid-message " m)
               ;; (mail who (tuple 'invalid-message))
               (loop players points rs)))))))
    ))
