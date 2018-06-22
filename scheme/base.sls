(library (scheme base)
  (export * + - ... / < <= = => > >= _
          abs and append apply assoc assq assv
          begin binary-port? boolean=? boolean?
          bytevector bytevector-append bytevector-copy bytevector-copy! bytevector-length bytevector-u8-ref bytevector-u8-set! bytevector?
          caar cadr call-with-current-continuation call-with-port call-with-values call/cc car case cdar cddr cdr ceiling char->integer
          char-ready? char<=? char<? char=? char>=? char>? char? close-input-port close-output-port close-port complex? cond
          cons case-lambda
          current-error-port current-input-port current-output-port
          define define-record-type define-syntax define-values denominator do dynamic-wind
          else eof-object eof-object? eq? equal? eqv? error
          even? exact exact-integer-sqrt
          exact-integer? exact? expt
          ;; error-object-irritants error-object-message
          cond-expand
          ;; error-object?
          features
          ;; file-error?
          ;; include-ci
          input-port-open?
          floor floor-quotient floor-remainder floor/ flush-output-port for-each
          gcd get-output-bytevector get-output-string guard
          if include
          inexact inexact?
          input-port? integer->char integer?
          lambda lcm length let let* let*-values let-syntax let-values letrec letrec* letrec-syntax list list->string list->vector list-copy
          list-ref list-set! list-tail list?
          make-bytevector make-list make-parameter make-string make-vector map max member memq memv min modulo
          negative? newline not null? number->string number? numerator
          odd? open-input-bytevector open-input-string open-output-bytevector
          open-output-string or output-port-open?
          output-port?
          pair? parameterize peek-char peek-u8 port? positive? procedure?
          quasiquote quote quotient
          raise raise-continuable rational? rationalize read-bytevector
          ;; read-bytevector!
          read-char ;; read-error?
          read-line read-string
          read-u8 real? remainder reverse round set! set-car! set-cdr! square
          string string->list string->number string->symbol string->utf8
          ;; string->vector
          string-append string-copy string-copy! string-fill!
          string-for-each string-length ;; string-map
          string-ref string-set!
          string<=? string<? string=? string>=? string>? string? substring
          symbol->string symbol=? symbol? syntax-error syntax-rules
          textual-port? truncate ;; truncate-quotient truncate-remainder
          ;; truncate/
          ;; u8-ready?
          unless unquote unquote-splicing utf8->string
          values vector vector->list ;; vector->string vector-append
          vector-append
          vector-copy
          vector-copy!
          vector-fill! vector-for-each
          vector-length vector-map vector-ref vector-set! vector?
          when with-exception-handler write-bytevector write-char
          write-string write-u8
          zero?
          import)
  (import (except (rename (chezscheme)
                          (error %error)
                          (assoc %assoc)
                          (member %member)
                          (bytevector-copy %bytevector-copy))
                  define-record-type)
          (srfi :9))
  (define (bytevector-append . bvs)
    (let* ((lengths (map bytevector-length bvs))
           (total (apply + lengths))
           (offsets lengths)
           (result (make-bytevector total 0))
           (i 0))
      (for-each (lambda (bv length)
                  ;; use old bytevector-copy!
                  ;; (display (format "bv: ~s, offset: ~s, length: ~s~%" bv i length)) (newline)
                  (bytevector-copy! bv 0 result i length)
                  (set! i (+ i length)))
                bvs
                lengths)
      result))
  (define bytevector-copy
    (case-lambda ((bv)
                  (%bytevector-copy bv))
                 ((bv start)
                  (bytevector-copy bv start (bytevector-length bv)))
                 ((bv start end)
                  (let ((n (make-bytevector (- end start))))
                    (bytevector-copy! bv start n 0 (- end start))
                    n))))
  (define vector-copy!
    (case-lambda ((to at from)
                  (vector-copy! to at from 0 (vector-length from)))
                 ((to at from start)
                  (vector-copy! to at from start (vector-length from)))
                 ((to at from start end)
                  (let loop ((i 0))
                    (if (= (+ start i) end)
                        (void)
                        (begin
                          (vector-set! to (+ at i) (vector-ref from (+ start i)))
                          (loop (+ i 1))))))))
  (define (vector-append . bvs)
    (let* ((lengths (map vector-length bvs))
           (total (apply + lengths))
           (offsets lengths)
           (result (make-vector total 0))
           (i 0))
      (for-each (lambda (bv length)
                  ;; use old vector-copy!
                  ;; (display (format "bv: ~s, offset: ~s, length: ~s~%" bv i length)) (newline)
                  (vector-copy! bv 0 result i length)
                  (set! i (+ i length)))
                bvs
                lengths)
      result))
  (define (list-set! lst index value)
    (set-car! (list-tail lst index) value))
  (define output-bytevector-getters (make-hash-table #t)) ;; key weak
  (define (open-output-bytevector)
    (let-values (((port getter) (open-bytevector-output-port)))
      (put-hash-table! output-bytevector-getters port getter)
      port))
  (define (get-output-bytevector port)
    (let ((q (get-hash-table output-bytevector-getters port #f)))
      (if q (q) (error 'get-output-bytevector "No output getter registered for port" port))))
  (define (floor/ n1 n2)
    (let ((nq (quotient n1 n2))
          (nr (remainder n1 n2)))
      (assert (= n1 (+ nr (* nq n2))))
      (values nq nr)))
  (define (floor-quotient n1 n2)
    (let ((nq (quotient n1 n2))
          (nr (remainder n1 n2)))
      (assert (= n1 (+ nr (* nq n2))))
      nq))
  (define (floor-remainder n1 n2)
    (let ((nq (quotient n1 n2))
          (nr (remainder n1 n2)))
      (assert (= n1 (+ nr (* nq n2))))
      nr))
  (define (exact-integer? n)
    (and (integer? n) (exact? n)))
  (define (error message . objects)
    (%error 'error message objects))
  (define write-u8
    (case-lambda ((octet)
                  (write-u8 octet (current-output-port)))
                 ((octet port)
                  (put-u8 port octet))))
  (define write-string
    (case-lambda ((string)
                  (write-string string (current-output-port) 0 (string-length string)))
                 ((string port)
                  (write-string string port 0 (string-length string)))
                 ((string port start)
                  (write-string string port start (string-length string)))
                 ((string port start end)
                  (put-string-some port string start (- end start)))))
  (define write-bytevector
    (case-lambda ((bytevector)
                  (write-bytevector bytevector (current-output-port) 0 (bytevector-length bytevector)))
                 ((bytevector port)
                  (write-bytevector bytevector port 0 (bytevector-length bytevector)))
                 ((bytevector port start)
                  (write-bytevector bytevector port start (bytevector-length bytevector)))
                 ((bytevector port start end)
                  (put-bytevector-some port bytevector start (- end start)))))
  (define read-u8
    (case-lambda (()
                  (read-u8 (current-input-port)))
                 ((port)
                  (get-u8 port))))
  (define read-string
    (case-lambda ((k)
                  (read-string k (current-input-port)))
                 ((k port)
                  (get-string-n port k))))
  (define read-line get-line)
  (define (square n)
    (* n n))
  (define read-bytevector
    (case-lambda ((k)
                  (read-bytevector k (current-input-port)))
                 ((k port)
                  (get-bytevector-n port k))))
  (define peek-u8
    (case-lambda (()
                  (peek-u8 (current-input-port)))
                 ((port)
                  (let ((c (get-u8 port)))
                    (unget-u8 port c)
                    c))))
  (define (open-input-bytevector data)
    (open-bytevector-input-port data))
  (define (input-port-open? p)
    (not (port-closed? p)))
  (define (output-port-open? p)
    (not (port-closed? p)))
  (define member
    (case-lambda ((obj list)
                  (%member obj list))
                 ((obj list compare)
                  (let loop ((lst list))
                    (if (null? lst)
                        #f
                        (if (compare obj (car lst))
                            lst
                            (loop (cdr lst))))))))
  (define assoc
    (case-lambda ((obj list)
                  (%assoc obj list))
                 ((obj list compare)
                  (let loop ((lst list))
                    (if (null? lst)
                        #f
                        (if (compare obj (caar lst))
                            (car lst)
                            (loop (cdr lst))))))))
  (define (features)
    ;; taken from chez-exe/utils.ss
    (define (os-name)
      (case (machine-type)
        ((a6fb ta6fb i3fb ti3fb) 'freebsd)
        ((a6le arm32le i3le ppc32le ta6le ti3le tppc32le) 'gnu-linux)
        ((a6nb i3nb ta6nb ti3nb) 'netbsd)
        ((a6nt i3nt ta6nt ti3nt) 'windows)
        ((a6ob i3ob ta6ob ti3ob) 'openbsd)
        ((a6osx i3osx ta6osx ti3osx) 'macosx)
        ((a6s2 i3s2 ta6s2 ti3s2) 'solaris)
        ((i3qnx) 'qnx)
        (else 'unknown-os)))
    (define (endianness)
      (case (machine-type)
        ((i3qnx a6s2 i3s2 ta6s2 ti3s2 a6osx i3osx ta6osx ti3osx a6ob i3ob ta6ob ti3ob a6nt i3nt ta6nt ti3nt a6nb i3nb ta6nb ti3nb a6le i3le ta6le ti3le a6fb ta6fb i3fb ti3fb) 'little-endian)
        ((arm32le tppc32le ppc32le) 'big-endian)
        (else 'unknown-endian)))
    (define (architecture)
      (case (machine-type)
        ((ta6s2 a6s2 ta6osx a6osx ta6ob a6ob ta6nt ta6nb a6nt a6nb ta6le a6le a6fb ta6fb) 'x86_64)
        ((i3qnx ti3s2 i3s2 ti3osx i3osx ti3ob i3ob ti3nt i3nt ti3nb i3nb i3le i3fb ti3fb ti3le) 'i386)
        ((arm32le) 'arm32)
        ((ppc32le tppc32le) 'ppc32)
        (else 'unknown-architecture)))
    (define (threading)
      (case (machine-type)
        ((ta6osx ti3osx ta6ob ti3ob ta6nt ti3nt ta6nb ti3nb ta6s2 ti3s2 ti3fb ta6fb ta6le ti3le tppc32le) 'threads)
        (else 'no-threads)))
    (append '(chez r7rs ratios complex ieee-float full-unicode syntax-rules full-numeric-tower)
            ;; ilp32 ...
            (list (os-name)
                  (architecture)
                  (threading)
                  (endianness))))

  (define-syntax cond-expand
    (syntax-rules (and or not else library scheme base chez)
      ((_)
       (syntax-error "Unfulfilled cond-expand"))
      ((_ (else body ...))
       (begin body ...))
      ((_ ((and) body ...) more-clauses ...)
       (begin body ...))
      ((_ ((and req1 req2 ...) body ...)
          more-clauses ...)
       (cond-expand (req1 (cond-expand ((and req2 ...) body ...)
                                       more-clauses ...))
                    more-clauses ...))
      ((_ ((or) body ...) more-clauses ...)
       (cond-expand more-clauses ...))
      ((_ ((or req1 req2 ...) body ...)
          more-clauses ...)
       (cond-expand (req1 (begin body ...))
                    (else (cond-expand ((or req2 ...) body ...)
                                       more-clauses ...))))
      ((_ ((not req) body ...)
          more-clauses ...)
       (cond-expand (req (cond-expand more-clauses ...))
                    (else body ...)))
      ;; ((_ (r7rs body ...)
      ;;               more-clauses ...)
      ;;  (begin body ...))
      ;; ((_ (chez body ...)
      ;;     more-clauses ...)
      ;;  (begin body ...))
      ((_ (chez
           body ...)
          more-clauses ...)
       (begin body ...))
      ((_ ((library (scheme base))
           body ...)
          more-clauses ...)
       (begin body ...))
      ;; Add clauses here for each library
      ((_ (feature-id body ...)
          more-clauses ...)
       (cond-expand more-clauses ...))
      ((_ ((library (name ...))
           body ...)
          more-clauses ...)
       (cond-expand more-clauses ...)))))
