#!chezscheme
(library (socket net)
  (export open-tcp-stream-socket
          open-tcp-server-socket
          tcp-server-connection-accept
          close-tcp-server-socket
          with-tcp-server-socket)
  
  (import (chezscheme) (socket ffi))
  
  (define-structure (socket fd))
  (define (make-socket-port socket)
    (let ((p (make-custom-binary-input/output-port (format "socket ~a" socket)
                                                   ;; read
                                                   (lambda (bv start n)
                                                     (recv socket bv start n #f))
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

  (define (open-tcp-stream-socket host-name port)
    (let ((sock (socket 'inet 'stream 'ip)))
      ;; TODO: resolve host-name to an address
      (setsockopt sock 1 'socket-option/reuseport #t)
      (setsockopt sock 1 'socket-option/reuseaddr #t)
      (if (connect sock (alloc-%sockaddr-in port host-name))
          (make-socket-port sock)
          (error 'open-tcp-stream-socket "Error connecting" host-name port sock))))

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
      (bind sock (alloc-%sockaddr-in port +inaddr-any+))
      (listen sock backlog-count)
      (make-socket sock)))

  (define (tcp-server-connection-accept socket)
    (let ((remote (alloc-%sockaddr-in 0 +inaddr-any+)))
      (let ((sock (accept4 (socket-fd socket) remote '())))
        (if sock
            (make-socket-port sock)
            #f)))))
