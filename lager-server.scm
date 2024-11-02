(import
 (owl toplevel)
 (lager decider)
 (lager const)
 (lager common))

;; (define (get-block fd len)
;;   (let ((bv (try-get-block fd len #f)))
;;     (if (= (bytevector-length bv) len)
;;         bv
;;         (bytevector-append bv (get-block fd (- len (bytevector-length bv)))))))

(define (make-client fd ip)
  (let ((thrname (string->symbol (str "client@" ip "-" (time-ns)))))
    (thread
     thrname
     (let loop ()
       (when (readable? fd)
         (let* ((bv (try-get-block fd 2 #t)))
           ;; eq? instead of = bc (bytevector-length) on #f yields #f and = on #f yields an (error)
           (if (eq? (bytevector-length bv) 2)
               (let* ((size (u16->n (bytevector->list bv)))
                      (res (reintern (fasl-decode (bytevector->list (try-get-block fd size #t)) (tuple 'bad)))))
                 (tuple-case res
                   ((bad)
                    (print "[client handler] invalid fasl received") ;; TODO: make sure this never happens
                    ;; (mail 'decider (tuple 'exiting!))
                    ;; (close-port fd)
                    ;; (kill thrname))
                    )
                   ((add-player! name thread)
                    (mail 'decider (tuple 'add-player! name thrname)))
                   (else
                    (mail 'decider res))))
               (let ()
                 (print "2 != 2 bailing out")
                 (mail 'decider (tuple 'exiting!))
                 (close-port fd)
                 (kill thrname)))))

       (if-lets ((_ v (maybe-next-mail)))
         (send-fasl fd v))

       (loop)))))

(λ (_)
  (let ((sock (open-socket *port*)))
    (thread
     'accepter
     (let loop ()
       (lets ((ip fd (tcp-client sock)))
         (make-client fd ip)
         (loop))))
    (thread 'decider (decider))))
