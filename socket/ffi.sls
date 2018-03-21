#!chezscheme
(library (socket ffi)
  (export socket
          +inaddr-any+
          make-sockaddr-in
          accept4
          bind
          connect
          listen
          sockaddr-in-address
          sockaddr-in-port
          send
          sendto
          recv
          recvfrom
          close
          getsockname
          getpeername
          getsockopt
          setsockopt
          alloc-%sockaddr-in
          %sockaddr-in-address
          %sockaddr-in-port
          getaddrinfo
          address-info-flags
          address-info-family
          address-info-socket-type
          address-info-address
          address-info-canonical-name
          address-info-next
          sockaddr-in-address
          sockaddr-in-port
          alloc-%addrinfo
          parse-and-free-addrinfo)
  (import (chezscheme))
  (define (activate-thread)
    #f;;((foreign-procedure "Sactivate_thread" () int))
    )
  (define (deactivate-thread)
    #f;;((foreign-procedure "Sdeactivate_thread" () void))
    )
;;;; TODO
  ;; - https://stackoverflow.com/questions/855544/is-there-a-way-to-flush-a-posix-socket
  ;; 2 shutdown
  ;; poll / select
  ;; sendfile?
  ;; (with-interrupts-disabled (f (#%$object-address bv (+ (foreign-sizeof 'ptr) 1 index)))
  ;; lock-object, unlock-object
  (define (extract-string/len p len)
    (define soc (foreign-sizeof 'char))
    (if (zero? p)
        #f
        (let loop ((i 0)
                   (lst '()))
          (if (= i len)
              (utf8->string (apply bytevector (reverse lst)))
              (loop (+ i 1)
                    (cons (foreign-ref 'unsigned-8 0 (+ p (* i soc))) lst))))))
  (define (extract-string p)
    (define soc (foreign-sizeof 'char))
    (if (zero? p)
        #f
        (let loop ((i 0)
                   (lst '()))
          (let ((c (foreign-ref 'unsigned-8 0 (+ p (* i soc)))))
            (if (= c 0)
                (utf8->string (apply bytevector (reverse lst)))
                (loop (+ i 1)
                      (cons c lst)))))))
  ;; taken from /usr/include/bits/socket.h
  (define (address-family->int symbol)
    (case symbol
      ((unspec unspecified unspecific) 0)
      ((unix local) 1)
      ((inet) 2)
      ((inet6) 10)
      (else (error 'address-family->int "Unknown address family" symbol))))

  (define (int->address-family int)
    (case int
      ((0) 'unspec)
      ((1) 'unix)
      ((2) 'inet)
      ((10) 'inet6)
      (else (error 'int->address-family "Unknown address family integer" int))))

  ;; taken from /usr/include/netdb.h
  (define (address-info-flags->int symbol)
    (if (list? symbol)
        (apply bitwise-ior (map address-info-flags->int symbol))
        (case symbol
          ((passive) 1)
          ((canonname) 2)
          ((numerichost) 4)
          ((v4mapped) 8)
          ((all) #x10)
          ((addrconfig) #x20)
          ((idn) #x40)
          ((canonidn) #x80)
          ((idn-allow-unassigned) #x100)
          ((idn-use-std3-ascii-rules) #x200)
          (else
           (error 'address-info-flags->int "Unknown address-info flag" symbol)))))
  (define (int->address-info-flags int)
    (filter (lambda (x) x)
            (map (lambda (el)
                   (let ((sym (car el))
                         (n (cadr el)))
                     (if (not (zero? (bitwise-and n int)))
                         sym
                         #f)))
                 '((passive 1)
                   (canonname 2)
                   (numerichost 4)
                   (v4mapped 8)
                   (all #x10)
                   (addrconfig #x20)
                   (idn #x40)
                   (canonidn #x80)
                   (idn-allow-unassigned #x100)
                   (idn-use-std3-ascii-rules #x200)))))

  ;; taken from /usr/include/bits/socket_type.h
  (define (socket-type->int symbol)
    (if (list? symbol)
        (apply bitwise-ior (map socket-type->int symbol))
        (case symbol
          ((stream) 1)
          ((dgram datagram) 2)
          ((raw) 3)
          ((rdm) 4)
          ((seqpacket) 5)
          ((dccp) 6)
          ((packet) 10)
          ((cloexec close-on-exec) #x80000)
          ((nonblock non-blocking) #x800)
          (else (error 'socket-type->int "Unknown socket type" symbol)))))
  (define (int->socket-type int)
    (case int
      ((1) 'stream)
      ((2) 'datagram)
      ((3) 'raw)
      ((4) 'rdm)
      ((5) 'seqpacket)
      ((6) 'dccp)
      ((10) 'packet)
      (else (error 'int->socket-type "Unknown socket type integer" int))))
  (define (accept-flags->int symbol)
    (if (list? symbol)
        (apply bitwise-ior (map socket-type->int symbol))
        (case symbol
          ((#f) 0)
          ((cloexec close-on-exec) #x80000)
          ((nonblock non-blocking) #x800)
          (else (error 'accept-flags->int "Unknown accept flags" symbol)))))

  ;; taken from /usr/include/linux/in.h
  (define (protocol->int symbol)
    (case symbol
      ((ip ip4 ipv4 #f) 0)
      ((icmp) 1)
      ((igmp) 2)
      ((ipip) 4)
      ((tcp) 6)
      ((egp) 8)
      ((pup) 12)
      ((udp) 17)
      ((idp) 22)
      ((tp) 29)
      ((dccp) 33)
      ((ipv6 ip6) 41)
      ((rsvp) 46)
      ((gre) 47)
      ((esp) 50)
      ((ah) 51)
      ((mtp) 92)
      ((beetph) 94)
      ((encap) 98)
      ((pim) 103)
      ((comp) 108)
      ((sctp) 132)
      ((udplite) 136)
      ((mpls) 137)
      ((raw) 255)
      (else (error 'protocol->int "Unknown protocol" symbol))))

  (define (msg-flag->int symbol)
    (if (list? symbol)
        (apply bitwise-ior (map msg-flag->int symbol))
        (case symbol
          ((#f) 0)
          ((oob) #x01)
          ((peek) #x02)
          ((dontroute tryhard) #x04)
          ((ctrunc) #x08)
          ((proxy) #x10)
          ((trunc) #x20)
          ((dontwait) #x40)
          ((eor) #x80)
          ((waitall) #x100)
          ((fin) #x200)
          ((syn) #x400)
          ((confirm) #x800)
          ((rst) #x1000)
          ((errqueue) #x2000)
          ((nosignal) #x4000)
          ((more) #x8000)
          ((waitforone) #x10000)
          ((batch) #x40000)
          ((fastopen) #x20000000)
          ((cmsg-cloexec) #x4000000)
          (else (error 'msg-flag->int "Unknown message flag" symbol)))))

  (define +inaddr-any+ 0)

  (define +eagain+ 11)

  (define (strerror errno)
    ((foreign-procedure "strerror" (int) string) errno))

  (define (errno)
    (foreign-ref 'int (foreign-entry "errno") 0))

  (define socket (case-lambda ((address-family socket-type)
                               (socket address-family socket-type 'ip))
                              ((address-family socket-type protocol)
                               (let ((res ((foreign-procedure "socket" (int int int) int)
                                           (address-family->int address-family)
                                           (socket-type->int socket-type)
                                           (protocol->int protocol))))
                                 (if (= res -1)
                                     (error 'socket "Error opening socket" address-family socket-type protocol)
                                     (begin
                                       ;; (format #t "socket: ~s~%" res)
                                       res))))))

  (define-ftype %sockaddr
    (struct (family unsigned-short)))
  (define (parse-%sockaddr p)
    (let ((family (int->address-family (ftype-ref %sockaddr (family) (make-ftype-pointer %sockaddr (ftype-pointer-address p))))))
      (case family
        ((inet)
         (parse-%sockaddr-in p))
        ((inet6)
         (parse-%sockaddr-in6 p))
        ((unix)
         (parse-%sockaddr-un p))
        (else
         (error 'parse-%sockaddr "Unknown family" family p)))))
  (define (%sockaddr-size p)
    (let ((family (int->address-family (ftype-ref %sockaddr (family) (make-ftype-pointer %sockaddr (ftype-pointer-address p))))))
      (case family
        ((inet) (ftype-sizeof %sockaddr-in))
        ((inet6) (ftype-sizeof %sockaddr-in6))
        ((unix) (ftype-sizeof %sockaddr-un))
        (else
         (error '%sockaddr-size "Unknown family" family p)))))
  (define-ftype %sockaddr-in
    (struct (family unsigned-short)
            (port (endian big unsigned-16))
            (address (endian big unsigned-32))
            (padding (array 8 char))))
  (define-structure (sockaddr-in family port address))
  (define (parse-%sockaddr-in p)
    (let ((p (make-ftype-pointer %sockaddr-in (ftype-pointer-address p))))
      (make-sockaddr-in (int->address-family (ftype-ref %sockaddr-in (family) p))
                        (ftype-ref %sockaddr-in (port) p)
                        (let ((a (ftype-ref %sockaddr-in (address) p)))
                          (list (bitwise-arithmetic-shift-right a 24)
                                (bitwise-and (bitwise-arithmetic-shift-right a 16) #xff)
                                (bitwise-and (bitwise-arithmetic-shift-right a 8) #xff)
                                (bitwise-and a #xff))))))
  (define-structure (sockaddr-in6 family port flow-info address scope-id))
  (define (parse-%sockaddr-in6 p)
    (let ((p (make-ftype-pointer %sockaddr-in6 (ftype-pointer-address p))))
      (make-sockaddr-in6 (int->address-family (ftype-ref %sockaddr-in6 (family) p))
                         (ftype-ref %sockaddr-in6 (port) p)
                         (ftype-ref %sockaddr-in6 (flow-info) p)
                         (let ((a0 (ftype-ref %sockaddr-in6 (address-0) p))
                               (a1 (ftype-ref %sockaddr-in6 (address-1) p))
                               (a2 (ftype-ref %sockaddr-in6 (address-2) p))
                               (a3 (ftype-ref %sockaddr-in6 (address-3) p)))
                           (list a0 a1 a2 a3))
                         (ftype-ref %sockaddr-in6 (scope-id) p))))
  (define-structure (sockaddr-un family path))
  (define-ftype %sockaddr-un
    (struct (family unsigned-short)
            (path (array 108 char))))
  (define-ftype %sockaddr-un%
    (struct (family unsigned-short)
            (path (array 108 char))))
  (define (parse-%sockaddr-un p)
    (let ((p (make-ftype-pointer %sockaddr-in6 (ftype-pointer-address p))))
      (make-sockaddr-un (int->address-family (ftype-ref %sockaddr-un (family) p))
                        (extract-string (ftype-&ref %sockaddr-un (path) p)))))

  (define (%sockaddr-in-port addr)
    (ftype-ref %sockaddr-in (port) addr))
  (define (%sockaddr-in-address addr)
    (ftype-ref %sockaddr-in (address) addr))
  (define (alloc-%sockaddr-in port address)
    (let ((res (make-ftype-pointer %sockaddr-in (foreign-alloc (ftype-sizeof %sockaddr-in)))))
      (ftype-set! %sockaddr-in (family) res (address-family->int 'inet))
      (ftype-set! %sockaddr-in (port) res port)
      (ftype-set! %sockaddr-in (address) res address)
      res))

  (define-ftype %addrinfo
    (struct (ai_flags int)
            (ai_family int)
            (ai_socktype int)
            (ai_protocol int)
            (ai_addrlen int)
            (ai_addr (* %sockaddr))
            (ai_canonname (* char))
            (ai_next (* %addrinfo))))
  (define (alloc-%addrinfo family socket-type protocol flags)
    (define size (ftype-sizeof %addrinfo))
    (let ((p (make-ftype-pointer %addrinfo (foreign-alloc size))))
      ((foreign-procedure "memset" (void* int size_t) void*) (ftype-pointer-address p) 0 size)
      (ftype-set! %addrinfo (ai_family) p (address-family->int family))
      (ftype-set! %addrinfo (ai_socktype) p (socket-type->int socket-type))
      (ftype-set! %addrinfo (ai_protocol) p (protocol->int protocol))
      (ftype-set! %addrinfo (ai_flags) p (address-info-flags->int flags))
      p))
  (define-structure (addrinfo foreign))
  (define-structure (address-info flags family socket-type address canonical-name next))
  (define (getaddrinfo name service hints)
    (let ((result (foreign-alloc (foreign-sizeof 'void*))))
      (let ((res ((foreign-procedure "getaddrinfo" (string string (* %addrinfo) void*) int)
                  name
                  service
                  (if hints (addrinfo-foreign hints) (make-ftype-pointer %addrinfo 0))
                  result)))
        (if (zero? res)
            (let ((ai (foreign-ref 'void* result 0)))
              (foreign-free result)
              (make-addrinfo (make-ftype-pointer %addrinfo ai)))
            (error 'getaddrinfo "Error in getaddrinfo" name service hints (gai-strerror res))))))
  (define (parse-and-free-addrinfo addrinfo)
    (let* ((p (addrinfo-foreign addrinfo))
           (result (parse-addrinfo p)))
      (freeaddrinfo addrinfo)
      result))
  (define (parse-addrinfo p)
    (if (ftype-pointer-null? p)
        #f
        (let* ((flags (int->address-info-flags (ftype-ref %addrinfo (ai_flags) p)))
               (family (int->address-family (ftype-ref %addrinfo (ai_family) p)))
               (socket-type (int->socket-type (ftype-ref %addrinfo (ai_socktype) p)))
               (address (parse-%sockaddr (ftype-ref %addrinfo (ai_addr) p)))
               (canonical-name (if (memq 'canonname flags) (extract-string (ftype-pointer-address (ftype-ref %addrinfo (ai_canonname) p))) #f))
               (next (parse-addrinfo (ftype-ref %addrinfo (ai_next) p))))
          (make-address-info flags family socket-type address canonical-name next))))
  (define (gai-strerror code)
    ((foreign-procedure "gai_strerror" (int) string) code))
  (define (freeaddrinfo addrinfo)
    ((foreign-procedure "freeaddrinfo" ((* %addrinfo)) void) (addrinfo-foreign addrinfo))) ;; why does (* addrinfo) not work here??

  ;; TODO: NI_NAMEREQD etc. from /usr/include/netdb.h
  (define (getnameinfo sockaddr)
    #f)

  (define-ftype %sockaddr-in6
    (struct (family unsigned-short)
            (port (endian big unsigned-16))
            (flow-info unsigned-32)
            (address-0 (endian big unsigned-32))
            (address-1 (endian big unsigned-32))
            (address-2 (endian big unsigned-32))
            (address-3 (endian big unsigned-32))
            (scope-id unsigned-32)))

  (define-ftype sockaddr-un
    (struct (family unsigned-short)
            (path (array 108 char))))

  (define (connect socket address)
    (let ((res ((foreign-procedure "connect" (int (* %sockaddr) int) int)
                socket
                (make-ftype-pointer %sockaddr (ftype-pointer-address address))
                (%sockaddr-size address))))
      (if (zero? res)
          #t
          (error 'connect "Error connecting socket" socket address (strerror (errno))))))

  (define (bind socket remote)
    (let ((res ((foreign-procedure "bind" (int (* %sockaddr) int) int)
                socket
                (make-ftype-pointer %sockaddr (ftype-pointer-address remote))
                (%sockaddr-size remote))))
      (if (zero? res)
          #t
          (error 'bind "Error binding socket" socket remote (strerror (errno))))))

  (define (listen socket backlog)
    (let ((res ((foreign-procedure "listen" (int int) int)
                socket backlog)))
      (if (zero? res)
          #t
          (error 'listen "Error listening on socket" socket backlog (strerror (errno))))))

  (define accept4
    (case-lambda
     ((socket remote)
      (accept4 socket remote #f))
     ((socket remote flags)
      (let ((size (foreign-alloc (ftype-sizeof int))))
        (when remote
          (foreign-set! 'int size 0 (%sockaddr-size remote)))
        (let ((res ((foreign-procedure __thread "accept4" (int (* %sockaddr) void* int) int)
                    socket
                    (make-ftype-pointer %sockaddr (if remote (ftype-pointer-address remote) 0))
                    (if remote size 0)
                    (accept-flags->int flags))))
          (if (= res -1)
              (if (= (errno) +eagain+)
                  #f
                  (error 'accept4 "Error accepting on socket" socket (strerror (errno))))
              (begin
                ;;(format #t "accept4 -> ~a~%" res)
                res)))))))

  (define send
    (case-lambda ((socket buffer)
                  (send socket buffer #f))
                 ((socket buffer flags)
                  (let ((res ((foreign-procedure __thread "send" (int u8* size_t int) ssize_t)
                              socket buffer (bytevector-length buffer) (msg-flag->int flags))))
                    (if (= res -1)
                        (error 'send "Error sending on socket" socket buffer flags (strerror (errno)))
                        res)))
                 ((socket buffer offset length flags)
                  ;; (let ((res (with-interrupts-disabled
                  ;;             (let ((p (#%$object-address buffer (+ (foreign-sizeof 'ptr) 1 offset))))
                  ;;               ((foreign-procedure "send" (int void* size_t int) ssize_t)
                  ;;                socket p length (msg-flag->int flags))))))
                  ;;   (if (= res -1)
                  ;;       (error 'send "Error sending on socket" socket buffer offset length flags (strerror (errno)))
                  ;;       res))
                  (let ((res (begin
                               (lock-object buffer)
                               (let* ((p (#%$object-address buffer (+ (foreign-sizeof 'ptr) 1 offset)))
                                      (f (msg-flag->int flags))
                                      (r (begin
                                           (deactivate-thread)
                                           ((foreign-procedure __thread "send" (int void* size_t int) ssize_t)
                                            socket p length f))))
                                 (activate-thread)
                                 (unlock-object buffer)
                                 r))))
                    (if (= res -1)
                        (error 'send "Error sending on socket" socket buffer offset length flags (strerror (errno)))
                        res)))))

  (define sendto
    (case-lambda ((socket remote buffer)
                  (sendto socket remote buffer 0 (bytevector-length buffer) #f))
                 ((socket remote buffer flags)
                  (let ((res ((foreign-procedure "sendto" (int u8* size_t int (* %sockaddr) int) ssize_t)
                              socket
                              buffer
                              (bytevector-length buffer)
                              (msg-flag->int flags)
                              (if remote (make-ftype-pointer %sockaddr (ftype-pointer-address remote)) 0)
                              (%sockaddr-size remote))))
                    (if (= res -1)
                        (error 'sendto "Error sending on socket" socket buffer flags (errno) (strerror (errno)))
                        res)))
                 ((socket remote buffer offset length flags)
                  ;; (let ((res (with-interrupts-disabled
                  ;;             (let ((p (#%$object-address buffer (+ (foreign-sizeof 'ptr) 1 offset))))
                  ;;               ((foreign-procedure "sendto" (int void* size_t int (* %sockaddr) int) ssize_t)
                  ;;                socket
                  ;;                p
                  ;;                length
                  ;;                (msg-flag->int flags)
                  ;;                (if remote (make-ftype-pointer %sockaddr (ftype-pointer-address remote)) 0)
                  ;;                (%sockaddr-size remote))))))
                  ;;   (if (= res -1)
                  ;;       (error 'sendto "Error sending on socket" socket buffer flags (errno) (strerror (errno)))
                  ;;       res))
                  (let ((res (begin
                               (lock-object buffer)
                               (deactivate-thread)
                               (let* ((p (#%$object-address buffer (+ (foreign-sizeof 'ptr) 1 offset)))
                                      (r ((foreign-procedure "sendto" (int void* size_t int (* %sockaddr) int) ssize_t)
                                          socket
                                          p
                                          length
                                          (msg-flag->int flags)
                                          (if remote (make-ftype-pointer %sockaddr (ftype-pointer-address remote)) 0)
                                          (%sockaddr-size remote))))
                                 (activate-thread)
                                 (unlock-object buffer)
                                 r))))
                    (if (= res -1)
                        (error 'sendto "Error sending on socket" socket buffer flags (errno) (strerror (errno)))
                        res)))))
  (define recv
    (case-lambda ((socket buffer)
                  (recv socket buffer #f))
                 ((socket buffer flags)
                  (let ((res ((foreign-procedure __thread "recv" (int u8* size_t int) ssize_t)
                              socket buffer (bytevector-length buffer) (msg-flag->int flags))))
                    (if (= res -1)
                        (error 'recv "Error receiving on socket" socket buffer flags (strerror (errno)))
                        res)))
                 ((socket buffer offset length flags)
                  ;; (let ((res (with-interrupts-disabled
                  ;;             (let ((p (#%$object-address buffer (+ (foreign-sizeof 'ptr) 1 offset))))
                  ;;               ((foreign-procedure "recv" (int void* size_t int) ssize_t)
                  ;;                socket p length (msg-flag->int flags))
                  ;;               ))))
                  ;;   (if (= res -1)
                  ;;       (if (= (errno) +eagain+)
                  ;;           #f
                  ;;           (error 'recv "Error receiving on socket" (errno) (strerror (errno)) socket buffer offset length flags ))
                  ;;       res))
                  (let ((res (begin
                               (lock-object buffer)
                               (let* ((p (#%$object-address buffer (+ (foreign-sizeof 'ptr) 1 offset)))
                                      (f (msg-flag->int flags))
                                      (r (begin
                                           (deactivate-thread)
                                           ((foreign-procedure __thread "recv" (int void* size_t int) ssize_t)
                                            socket p length f))))
                                 (activate-thread)
                                 (unlock-object buffer)
                                 r))))
                    (if (= res -1)
                        (if (= (errno) +eagain+)
                            #f
                            (error 'recv "Error receiving on socket" (errno) (strerror (errno)) socket buffer offset length flags ))
                        (begin
                          ;; (format #t "recv ~a of ~a at offset ~a on ~s~%" res length offset (utf8->string buffer))
                          res))))))
  (define recvfrom
    (case-lambda ((socket remote buffer)
                  (recvfrom socket remote buffer #f))
                 ((socket remote buffer flags)
                  (let ((size (foreign-alloc (ftype-sizeof int))))
                    (when remote
                      (foreign-set! 'int size 0 (%sockaddr-size remote)))
                    (let ((res ((foreign-procedure "recvfrom" (int u8* size_t int (* %sockaddr) void*) ssize_t)
                                socket buffer (bytevector-length buffer) (msg-flag->int flags)
                                (if remote (ftype-pointer-address remote) 0)
                                (if remote size 0))))
                      (if (= res -1)
                          (error 'recvfrom "Error receiving on socket" socket buffer flags (strerror (errno)))
                          res))))
                 ((socket remote buffer offset length flags)
                  (let ((size (foreign-alloc (ftype-sizeof int))))
                    (when remote
                      (foreign-set! 'int size 0 (%sockaddr-size remote)))
                    ;; (let ((res (with-interrupts-disabled
                    ;;             (let ((p (#%$object-address buffer (+ (foreign-sizeof 'ptr) 1 offset))))
                    ;;               ((foreign-procedure "recvfrom" (int void* size_t int (* %sockaddr) void*) ssize_t)
                    ;;                socket p length (msg-flag->int flags)
                    ;;                (if remote (ftype-pointer-address remote) 0)
                    ;;                (if remote size 0))))))
                    ;;   (if (= res -1)
                    ;;       (error 'recvfrom "Error receiving on socket" socket buffer flags (strerror (errno)))
                    ;;       res))
                    (let ((res (begin
                                 (lock-object buffer)
                                 (let* ((p (#%$object-address buffer (+ (foreign-sizeof 'ptr) 1 offset)))
                                        (f (msg-flag->int flags))
                                        (rem (if remote (ftype-pointer-address remote) 0))
                                        (rem-size (if remote size 0))
                                        (r (begin
                                             (deactivate-thread)
                                             ((foreign-procedure "recvfrom" (int void* size_t int (* %sockaddr) void*) ssize_t)
                                              socket p length f
                                              rem
                                              rem-size))))
                                   (activate-thread)
                                   (unlock-object buffer)
                                   r))))
                      (if (= res -1)
                          (error 'recvfrom "Error receiving on socket" socket buffer flags (strerror (errno)))
                          res))))))

  (define (close socket)
    (let ((res ((foreign-procedure "close" (int) int) socket)))
      (if (zero? res)
          #t
          (error 'close "Error closing socket" socket (strerror (errno))))))

  (define (getsockname socket address)
    (let ((size (foreign-alloc (ftype-sizeof int))))
      (foreign-set! 'int size 0 (%sockaddr-size address))
      (let ((res ((foreign-procedure "getsockname" (int (* %sockaddr) void*) int)
                  socket
                  (make-ftype-pointer %sockaddr (ftype-pointer-address address))
                  size)))
        (if (zero? res)
            address
            (error 'getsockname "Error in getsockname" socket address (strerror (errno)))))))

  (define (getpeername socket address)
    (let ((size (foreign-alloc (ftype-sizeof int))))
      (foreign-set! 'int size 0 (%sockaddr-size address))
      (let ((res ((foreign-procedure "getpeername" (int (* %sockaddr) void*) int)
                  socket
                  (make-ftype-pointer %sockaddr (ftype-pointer-address address))
                  size)))
        (if (zero? res)
            address
            (error 'getpeername "Error in getpeername" socket address (strerror (errno)))))))

  (define (getsockopt socket level optname)
    (define f (foreign-procedure "getsockopt" (int int int void* void*) int))
    (define (int opt-int)
      (let ((i (foreign-alloc (ftype-sizeof int)))
            (size (foreign-alloc (ftype-sizeof int))))
        (let ((res (f socket level opt-int i size)))
          (if (zero? res)
              (let ((v (foreign-ref 'int i 0)))
                (foreign-free i)
                (foreign-free size)
                i)
              (error 'getsockopt "Error on getsockopt" socket level optname (strerror (errno)))))))
    (define (bool opt-int)
      (let ((i (foreign-alloc (ftype-sizeof int)))
            (size (foreign-alloc (ftype-sizeof int))))
        (let ((res (f socket level opt-int i size)))
          (if (zero? res)
              (let ((v (foreign-ref 'int i 0)))
                (foreign-free i)
                (foreign-free size)
                (zero? i))
              (error 'getsockopt "Error on getsockopt" socket level optname (strerror (errno)))))))
    (case optname
      ;; based on /usr/include/asm-generic/socket.h
      ((socket-option/debug) (bool 1))
      ((socket-option/reuseaddr) (bool 2))
      ((socket-option/dontroute) (bool 5))
      ((socket-option/broadcast) (bool 6))
      ((socket-option/sndbuf) (int 7))
      ((socket-option/rcvbuf) (int 8))
      ((socket-option/keepalive) (bool 9))
      ((socket-option/oobinline) (bool 10))
      ((socket-option/reuseport) (bool 15))
      ((socket-option/rcvlowat) (int 18))
      ((socket-option/sndlowat) (int 19))
      (else (error 'getsockopt "Unknown socket option" socket level optname))))

  (define (setsockopt socket level optname optval)
    (define f (foreign-procedure "setsockopt" (int int int void* int) int))
    (define (int opt-int)
      (let ((i (foreign-alloc (ftype-sizeof int)))
            (size (foreign-alloc (ftype-sizeof int))))
        (foreign-set! 'int i 0 optval)
        (let ((res (f socket level opt-int i size)))
          (if (zero? res)
              (let ((v (foreign-ref 'int i 0)))
                (foreign-free i)
                (foreign-free size)
                i)
              (error 'setsockopt "Error on setsockopt" socket level optname optval (strerror (errno)))))))
    (define (bool opt-int)
      (let ((i (foreign-alloc (ftype-sizeof int)))
            (size (ftype-sizeof int)))
        (foreign-set! 'int i 0 (if optval 1 0))
        ;; (display (format "setting boolean socket option ~a to ~a~%" opt-int optval))
        (let ((res (f socket level opt-int i size)))
          (if (zero? res)
              #t
              (error 'setsockopt "Error on setsockopt" socket level optname optval (strerror (errno)))))))
    (case optname
      ;; based on /usr/include/asm-generic/socket.h
      ((socket-option/debug) (bool 1))
      ((socket-option/reuseaddr) (bool 2))
      ((socket-option/dontroute) (bool 5))
      ((socket-option/broadcast) (bool 6))
      ((socket-option/sndbuf) (int 7))
      ((socket-option/rcvbuf) (int 8))
      ((socket-option/keepalive) (bool 9))
      ((socket-option/oobinline) (bool 10))
      ((socket-option/reuseport) (bool 15))
      ((socket-option/rcvlowat) (int 18))
      ((socket-option/sndlowat) (int 19))
      (else (error 'setsockopt "Unknown socket option" socket level optname optval))))

  (define-ftype %protoent
    (struct (name void*)
            (aliases void*)
            (proto int)))
  (define (getprotobyname name)
    ((foreign-procedure "getprotobyname" (string) (* %protoent))
     name))
  (define-ftype %servent
    (struct (name void*)
            (aliases void*)
            (port (endian big unsigned-16)) ;; should really be integer, but that does not work!
            (proto void*)))
  (define (getservbyname name proto)
    ((foreign-procedure "getservbyname" (string string) (* %servent))
     name proto))
  (define (getservbyport port proto)
    ((foreign-procedure "getservbyport" (int string) (* %servent))
     port proto))
  (define (getservent)
    ((foreign-procedure "getservent" () (* %servent))))
  (define (setservent stay-open?)
    ((foreign-procedure "setservent" (boolean) void)
     stay-open?))
  (define (endservent)
    ((foreign-procedure "endservent" () void)))
  (define-structure (service-entry name aliases port protocol))
  (define (parse-%servent p)
    (if (not (ftype-pointer-null? p))
        (let ((name (extract-string (ftype-ref %servent (name) p)))
              (protocol (extract-string (ftype-ref %servent (proto) p)))
              (port (ftype-ref %servent (port) p))
              (aliases '(TODO)))
          (make-service-entry name aliases port protocol))
        #f))
  (define (ipv4 one two three four)
    (+ (* one 256 256 256)
       (* two 256 256)
       (* three 256)
       four))
  (load-shared-object #f))

;; macos port options
;; SO_DEBUG: 1
;; SO_REUSEPORT: 512
;; SO_KEEPALIVE: 8
;; SO_DONTROUTE: 16
;; SO_LINGER: 128
;; SO_BROADCAST: 32
;; SO_OOBINLINE: 256
;; SO_SNDBUF: 4097
;; SO_RCVBUF: 4098
;; SO_SNDLOWAT: 4099
;; SO_SNDTIMEO: 4101
;; SO_RCVTIMEO: 4102
;; SO_TYPE: 4104
;; SO_ERROR: 4103
;; SO_NOSIGPIPE: 4130
;; SO_NREAD: 4128
;; SO_NWRITE: 4132
;; SO_LINGER_SEC: 4224
