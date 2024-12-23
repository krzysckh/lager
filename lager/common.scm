(define-library (lager common)
  (import
   (owl toplevel)
   (raylib)
   (lager const))

  (export
   maybe-next-mail
   mail*
   player-threads
   lset*
   mesgof
   killed?
   get-mailq
   random-point
   random-points
   edit-player
   u16->n
   n->u16
   reintern
   die
   number->bot-name
   send-fasl
   )

  (begin
    (define (maybe-next-mail)
      (let ((envelope (check-mail)))
        (if (tuple? envelope)
            (values (ref envelope 1) (ref envelope 2))
            (values #f #f))))

    (define (mail* targets v)
      (if (null? targets)
          #t
          (begin
            (mail (car targets) v)
            (mail* (cdr targets) v))))

    (define (player-threads l)
      (map cadr l))

    (define (lset* l where what)
      (if (>= where (len l))
          (append l (list what))
          (lset l where what)))

    (define (mesgof sym q)
      (filter (λ (x) (eq? (ref x 1) sym)) q))

    (define (killed? l player-name)
      (has? (map (λ (t) (car* (ref t 2))) l) player-name))

    (define (get-mailq)
      (let loop ((acc #n))
        (lets ((_ v (maybe-next-mail)))
          (if v
              (loop (append acc (list v)))
              acc))))


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

    (define (edit-player players player f)
      (let loop ((acc #n) (players players))
        (cond
         ((null? players) acc)
         ((string=? (car player) (caar players))
          (loop (append acc (list (f (car players)))) (cdr players)))
         (else
          (loop (append acc (list (car players))) (cdr players))))))

    (define (u16->n l) (bior (<< (car l) 8) (cadr l)))
    (define (n->u16 n) (list (band (>> n 8) #xff) (band n #xff)))

    (define (reintern tuple)
      (list->tuple (map (λ (x) (if (symbol? x) (string->symbol (symbol->string x)) x)) (tuple->list tuple))))

    (define (die)
      (close-window)
      (catch-thread '_))

    (define (number->bot-name i)
      (string-append "local-bot@" (number->string i)))

    (define (send-fasl fd v)
      (let ((fasl (fasl-encode v)))
        (write-bytes fd (n->u16 (len fasl)))
        (write-bytes fd fasl)))

    ))
