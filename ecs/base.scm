;; don't use define-record, to actually get the record type descriptor into `aspect'

(define aspect (make-record-type "aspect" '(name type data)))
(define aspect? (record-predicate aspect))
(define aspect-name (record-accessor aspect 0))
(define aspect-type (record-accessor aspect 1))
(define aspect-data (record-accessor aspect 2))
(define set-aspect-data! (record-mutator aspect 2))
(define make-aspect (record-constructor aspect))
;;;; vector aspects
(define (aspect-ref aspect entity) (vector-ref (aspect-data aspect) entity))
(define (aspect-set! aspect entity value) (vector-set! (aspect-data aspect) entity value))
(define (grow-aspect! aspect new-size)
  (let ((new-vector (make-vector new-size #f))
        (old-vector (aspect-data aspect)))
    (let loop ((i 0))
      (if (>= i (vector-length old-vector))
          (set-aspect-data! aspect new-vector)
          (begin
            (vector-set! new-vector i (vector-ref old-vector i))
            (loop (+ i 1)))))))
(define (create-aspect-data)
  (make-vector *current-aspect-size* #f))
(define (aspect-remove! aspect entity)
  (aspect-set! aspect entity #f))
(define for-all-entities
  (let ()
    (case-lambda ((fun aspect)
                  (define i 0)
                  (vector-for-each (lambda (data)
                                     (when data
                                       (fun i))
                                     (set! i (+ i 1)))
                                   (aspect-data aspect)))
                 ((fun aspect0 aspect1)
                  (define i 0)
                  (vector-for-each (lambda (one two)
                                     (when (and one two)
                                       (fun i))
                                     (set! i (+ i 1)))
                                   (aspect-data aspect0) (aspect-data aspect1)))
                 ;; it seems, vector-for-each is only optimised for 1 and 2 vectors
                 ;; ((fun aspect0 aspect1 aspect2)
                 ;;  (define i 0)
                 ;;  (vector-for-each (lambda (one two three)
                 ;;                     (when (and one two three)
                 ;;                       (fun i))
                 ;;                     (set! i (+ i 1)))
                 ;;                   (aspect-data aspect0) (aspect-data aspect1) (aspect-data aspect2)))
                 ((fun . aspects)
                  (define i 0)
                  (apply vector-for-each (lambda data
                                           (when (every (lambda (x) x) data)
                                             (fun i))
                                           (set! i (+ i 1)))
                         (map aspect-data aspects))))))
;;;; end vector aspects
;;;; hashmap aspects
;; (define (aspect-ref aspect entity) (get-hash-table (aspect-data aspect) entity #f))
;; (define (aspect-set! aspect entity value) (put-hash-table! (aspect-data aspect) entity value))
;; (define (grow-aspect! aspect new-size)
;;   new-size
;;   aspect)
;; (define (create-aspect-data)
;;   (make-hash-table))
;; (define (aspect-remove! aspect entity)
;;   (remove-hash-table! (aspect-data aspect) entity))
;; (define for-all-entities
;;   (let ()
;;     (case-lambda ((fun aspect)
;;                   (define i 0)
;;                   ;; (vector-for-each (lambda (data)
;;                   ;;                    (when data
;;                   ;;                      (fun i))
;;                   ;;                    (set! i (+ i 1)))
;;                   ;;                  (aspect-data aspect))
;;                   (hash-table-for-each (aspect-data aspect)
;;                                        (lambda (key data)
;;                                          (when data
;;                                            (fun key))
;;                                          (set! i (+ i 1))))
;;                   )
;;                  ((fun . aspects)
;;                   (for-each (lambda (index)
;;                               (when (every (lambda (aspect) (aspect-ref aspect index)) aspects)
;;                                 (fun index)))
;;                             (remove-duplicates (apply append (map vector->list (map hashtable-keys (map aspect-data aspects)))))
;;                             ;; (map hashtable-keys (map aspect-data aspects))
;;                             )))))
;;;; end hashmap aspects
(define *next-entity-id* 0)
(define (entity-count) (- *next-entity-id* (free-entity-ids 'length)))
(define make-queue
  (let-syntax ((push (syntax-rules ()
                       ((push obj lst)
                        (set! lst (cons obj lst)))))
               (pop (syntax-rules ()
                      ((pop lst)
                       (if (null? lst)
                           #f
                           (let ((t (car lst)))
                             (set! lst (cdr lst))
                             t))))))
    (lambda ()
      (let ((front '()) (back '()))
        (lambda (cmd . data)
          (define exchange
            (lambda ()
              (set! front (reverse back))
              (set! back '())))
          (case cmd
            ((push) (push (car data) back))
            ((pop) (or (pop front)
                       (begin
                         (exchange)
                         (pop front))))
            ((contains) (or (member (car data) front)
                            (member (car data) back)))
            ((length) (+ (length front) (length back)))
            ;; ((peek) (unless (pair? front)
            ;;           (exchange))
            ;;  (car front))
            ;; ((show) (format #t "~s\n" (append front (reverse back))))
            ;; ((fb) (format #t "front: ~s, back: ~s\n" front back))
            ;; (else (error "Illegal cmd to queue object" cmd))
            ))))))
(define free-entity-ids (make-queue))
(define (next-entity-id)
  (or (free-entity-ids 'pop)
      (let ((id *next-entity-id*))
        (set! *next-entity-id* (+ *next-entity-id* 1))
        id)))
(define (make-entity!)
  (when (>= *next-entity-id* *current-aspect-size*)
    (grow-aspects!))
  (next-entity-id))
(define (destroy-entity! entity)
  (hash-table-for-each *aspects*
                       (lambda (name aspect)
                         (aspect-remove! aspect entity)))
  (unless (free-entity-ids 'contains entity)
    (free-entity-ids 'push entity)))
(define (aspect-fields aspect) (vector->list (record-type-field-names (aspect-type aspect))))
(define (grow-aspects!)
  (let ((new-size (exact (round (* *current-aspect-size* 1.5)))))
    (hash-table-for-each *aspects*
                         (lambda (name aspect)
                           (grow-aspect! aspect new-size)))
    (set! *current-aspect-size* new-size)))

(define *aspects* (make-hash-table))
(define (find-aspect name)
  (get-hash-table *aspects* name #f))
(define (register-aspect! name aspect)
  (put-hash-table! *aspects* name aspect))
(define *current-aspect-size* 8)
(define construct-name
  (lambda (template-identifier . args)
    (datum->syntax
     template-identifier
     (string->symbol
      (apply string-append
             (map (lambda (x)
                    (if (string? x)
                        x
                        (symbol->string (syntax->datum x))))
                  args))))))


;; optimized with case-lambda :-/

(define (remove-duplicates lst)
  (let loop ((lst (sort <
                        lst))
             (res '()))
    (if (null? lst)
        (reverse res)
        (if (member (car lst) res)
            (loop (cdr lst) res)
            (loop (cdr lst) (cons (car lst) res))))))



(define *events* (make-hash-table))
(define (register-event! name function)
  (put-hash-table! *events* name (cons function (get-hash-table *events* name '()))))
(define (execute-events!)
  (hash-table-for-each *events*
                       (lambda (name functions)
                         (for-each (lambda (f) (f)) functions)))
  (set! *events* (make-hash-table)))
(define (all-aspects entity)
  (let ((result '()))
    (hash-table-for-each *aspects*
                         (lambda (name aspect)
                           (let ((value (aspect-ref aspect entity)))
                             (when value
                               (set! result (cons (cons name value) result))))))
    result))
