(define-library (lager const)
  (import
   (owl toplevel))

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
   )

  (begin
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
    ;; in camera coords
    (define map:pad 20)
    (define map:sz 150)

    (define *port* 8855)

    ))