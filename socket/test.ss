(import (socket net))

(define (test-client)
  (define (ipv4 one two three four)
    (+ (* one 256 256 256)
       (* two 256 256)
       (* three 256)
       four))
  (define socket (open-tcp-stream-socket (ipv4 172 217 16 68) 80))
  (put-bytevector socket (string->utf8 "GET / HTTP/1.1\r\n\r\n"))
  (display (utf8->string (get-bytevector-some socket))))

(define (test-server port)
  (with-tcp-server-socket port 1
                          (lambda (socket)
                            (let ((p (tcp-server-connection-accept socket)))
                              (let loop ((in (get-bytevector-some p)))
                                (if (eof-object? in)
                                    'done
                                    (begin
                                      (put-bytevector p in)
                                      (loop (get-bytevector-some p)))))))))
