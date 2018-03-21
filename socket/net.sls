#!chezscheme
(library (socket net)
  (export open-tcp-stream-socket
          open-tcp-server-socket
          tcp-server-connection-accept
          close-tcp-server-socket
          with-tcp-server-socket)

  (import (chezscheme) (socket ffi) (socket ssl-ffi))

  (define-structure (socket fd))
  (define (make-socket-port socket)
    (let ((p (make-custom-binary-input/output-port (format "socket ~a" socket)
                                                   ;; read
                                                   (lambda (bv start n)
                                                     (let ((i (recv socket bv start n #f)))
                                                       ;;(format #t "read ~a from ~a~%" i socket)
                                                       ;; (format #t "~s ~s ~s~%" start n (utf8->string bv))
                                                       i))
                                                   ;; write
                                                   (lambda (bv start n)
                                                     (send socket bv start n #f))
                                                   ;; get-position
                                                   #f
                                                   ;; set-position!
                                                   #f
                                                   (lambda ()
                                                     (close socket))
                                                   )))
      p))
  (define (make-ssl-socket-port bio)
    (let ((p (make-custom-binary-input/output-port (format "ssl socket ~a" bio)
                                                   ;; read
                                                   (lambda (bv start n)
                                                     ;; TODO: handle retrying better
                                                     (let ((res (bio-read bio bv start n)))
                                                       (if (= -1 res)
                                                           (when (bio-should-retry bio)
                                                             (sleep (make-time 'time-duration 5000000 0))
                                                             (bio-read bio bv start n))
                                                           res)))
                                                   ;; write
                                                   (lambda (bv start n)
                                                     ;; TODO: handle retrying at all
                                                     (bio-write bio bv start n))
                                                   ;; get-position
                                                   #f
                                                   ;; set-position!
                                                   #f
                                                   (lambda ()
                                                     #f
                                                     ;; TODO
                                                     ;;(close socket)
                                                     )
                                                   )))
      p))

  (define open-tcp-stream-socket
    (case-lambda ((host-name port)
                  (open-tcp-stream-socket host-name port #f))
                 ((host-name port ssl?)
                  (if ssl?
                      (open-tcp-ssl-stream-socket host-name port)
                      (open-tcp-plain-stream-socket host-name port)))))
  (define (ipv4 one two three four)
  (+ (* one 256 256 256)
     (* two 256 256)
     (* three 256)
     four))
  (define (open-tcp-plain-stream-socket host-name port)
    (let* ((sockaddr (address-info-address (parse-and-free-addrinfo (getaddrinfo host-name (format "~a" port) #f)))))
      (let ((sock (socket 'inet 'stream 'ip)))
        ;; TODO: resolve host-name to an address
        (setsockopt sock 1 'socket-option/reuseport #t)
        (setsockopt sock 1 'socket-option/reuseaddr #t)
        (if (connect sock (alloc-%sockaddr-in (sockaddr-in-port sockaddr) (apply ipv4 (sockaddr-in-address sockaddr))))
            (make-socket-port sock)
            (error 'open-tcp-stream-socket "Error connecting" host-name port sock)))))

  (define (open-tcp-ssl-stream-socket host-name port)
    (define ctx (ssl-ctx-new (tls-client-method)))
    (ssl-ctx-load-verify-locations ctx "/etc/ssl/certs/ca-certificates.crt" #f)
    (let* ((bio (bio-new-ssl-connect ctx))
           (ssl (bio-get-ssl bio)))
      (ssl-set-mode ssl ssl-mode-auto-retry)
      (bio-set-conn-hostname bio (format "~a:~a" host-name port))
      (bio-do-connect bio)
      ;; verify
      (make-ssl-socket-port bio)))

  (define (with-tcp-server-socket port backlog-count fun)
    (define p #f)
    (dynamic-wind
        (lambda () (set! p (open-tcp-server-socket port backlog-count)))
        (lambda () (fun p))
        (lambda () (close-tcp-server-socket p))))

  (define (close-tcp-server-socket socket)
    (close (socket-fd socket)))

  (define (open-tcp-server-socket port backlog-count)
    (let ((sock (socket 'inet 'stream 'ip)))
      (setsockopt sock 1 'socket-option/reuseport #t)
      (setsockopt sock 1 'socket-option/reuseaddr #t)
      (bind sock (alloc-%sockaddr-in port +inaddr-any+))
      (listen sock backlog-count)
      (make-socket sock)))

  (define (tcp-server-connection-accept socket)
    (let ((remote (alloc-%sockaddr-in 0 +inaddr-any+)))
      (let ((sock (accept4 (socket-fd socket) remote '())))
        (if sock
            (make-socket-port sock)
            #f)))))
