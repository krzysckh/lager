(define-library (lager const)
  (import
   (owl toplevel)
   (scheme process-context))

  (export
   *window-size*
   *cam-offset*
   *grid-size*
   *point-radius*
   *frames-per-pos*
   *speed-base*
   *map-mult*
   *map-size*
   *n-points*
   dgl:min
   dgl:max
   map:pad
   map:sz
   *port*
   *n-bots*
   *default-srv*
   weblager?
   )

  (begin
    (define *window-size* 600)

    (define *cam-offset* (list (/ *window-size* 2) (/ *window-size* 2)))

    (define *grid-size* 50)
    (define *point-radius* 10)
    (define *frames-per-pos* 8)

    (define *speed-base* 100)
    (define *map-mult* 2)
    (define *map-size* (* *window-size* 4))

    (define *n-points* 128)

    (define dgl:min (- 0 *map-size*))
    (define dgl:max *map-size*)
    ;; in camera coords
    (define map:pad 20)
    (define map:sz 150)

    (define _*port* 8855)
    (define *weblager?* (string=? (last (command-line) "") "WEBLAGER"))
    (define (weblager?) *weblager?*)

    (define *port* (if (weblager?) (+ _*port* 1) _*port*))

    (define *n-bots* 10)
    ;; (define *default-srv* "localhost")
    (define *default-srv* "pub.krzysckh.org")

    ))
