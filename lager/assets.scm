(define-library (lager assets)
  (import
   (owl toplevel)
   (raylib)
   (lager const))

  (export
   get-asset
   add-asset
   asset
   start-asset-handler
   initialize-default-assets
   )

  (begin
    (define font (list->bytevector (file->list "third-party/0xproto.otf")))

    (define (start-asset-handler)
      (thread
       'assets
       (let loop ((l #n))
         (lets ((who v (next-mail)))
           (tuple-case v
             ((get sym)
              (mail who (cdr* (assoc sym l)))
              (loop l))
             ((set key value)
              (mail who 'ok)
              (loop (append l (list (cons key value)))))
             (else
              (loop l)))))))

    (define (add-asset k v)
      (interact 'assets (tuple 'set k v)))

    (define (get-asset k)
      (interact 'assets (tuple 'get k)))

    (define asset get-asset)

    (define (initialize-default-assets)
      (add-asset 'font (bytevector->font font".otf" 16 1024))
      (add-asset 'font32 (bytevector->font font".otf" 32 1024))
      (add-asset 'font64 (bytevector->font font".otf" 64 1024))
      )

    ))
