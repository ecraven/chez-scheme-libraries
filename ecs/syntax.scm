(define-syntax
  define-aspect
  (lambda (x)
    (syntax-case x ()
      ((_ name field-name ...)
       (with-syntax ((struct-name (construct-name (syntax name) "%" (syntax name)))
                     (name-string (datum->syntax (syntax name) (symbol->string (syntax->datum (syntax name)))))
                     (maker (construct-name (syntax name) "make-" (syntax name)))
                     (setter (construct-name (syntax name) "set-" (syntax name) "!"))
                     (predicate (construct-name (syntax name) (syntax name) "?"))
                     ((access ...)
                      (map (lambda (x) (construct-name x (syntax name) "-" x))
                           (syntax (field-name ...))))
                     ((internal-access ...)
                      (let ((fields (syntax (field-name ...))))
                        (map (lambda (field-name) (with-syntax ((index (list-index (lambda (el) (eq? el (syntax->datum field-name))) (map syntax->datum fields))))
                                                          (syntax (record-accessor (aspect-type name) index))))
                             fields)))
                     ((assign ...)
                      (map (lambda (x) (construct-name x "set-" (syntax name) "-" x "!"))
                           (syntax (field-name ...))))
                     ((internal-assign ...)
                      (let ((fields (syntax (field-name ...))))
                        (map (lambda (field-name) (with-syntax ((index (list-index (lambda (el) (eq? el (syntax->datum field-name))) (map syntax->datum fields))))
                                                          (syntax (record-mutator (aspect-type name) index))))
                             fields))))
                    (syntax
                     (begin
                       (define name (make-aspect 'name (make-record-type name-string '(field-name ...)) (create-aspect-data)))
                       (define maker (record-constructor (aspect-type name)))
                       (define (setter entity value)
                         (aspect-set! name entity value))
                       (define (predicate entity)
                         (not (not (aspect-ref name entity))))
                       (register-aspect! 'name name)
                       (define access
                         (let ((acc internal-access))
                           (lambda (entity)
                             (let ((aspect (aspect-ref name entity)))
                               (unless aspect
                                 (error 'access "Entity does not have aspect." entity 'name))
                               (acc aspect)))))
                       ...
                       (define assign
                         (let ((mutate internal-assign))
                           (lambda (entity value)
                             (let ((aspect (aspect-ref name entity)))
                               (unless aspect
                                 (error 'assign "Entity does not have aspect." entity 'name))
                               (mutate aspect value)))))
                       ...
                       )))
       ))))

(define-syntax
  define-entity
  (lambda (x)
    (syntax-case x ()
      ((_ name (aspect-name ...))
       (with-syntax ((maker (construct-name (syntax name) "make-" (syntax name)))

                     ((set-aspect ...) (map (lambda (aspect)
                                              (construct-name (syntax name) "set-" aspect "!"))
                                            (syntax (aspect-name ...))))
                     ((make-aspect ...) (map (lambda (aspect)
                                               (construct-name (syntax name) "make-" aspect))
                                             (syntax (aspect-name ...))))
                     ((aspect-params ...)
                      (map (lambda (aspect)
                             (let ((names (aspect-fields (find-aspect (syntax->datum aspect)))))
                               (map (lambda (field-name)
                                      (construct-name (syntax name) (syntax->datum aspect) "-" field-name))
                                    names)))
                           (syntax (aspect-name ...))))
                     (params
                      (apply append (map (lambda (aspect)
                                           (let ((names (aspect-fields (find-aspect (syntax->datum aspect)))))
                                             (map (lambda (field-name)
                                                    (construct-name (syntax name) (syntax->datum aspect) "-" field-name))
                                                  names)))
                                         (syntax (aspect-name ...))))))
                    (syntax
                     (define (maker . params)
                       (define entity (make-entity!))
                       (set-aspect entity (make-aspect . aspect-params))
                       ...
                       entity)))))))
(define-syntax
  define-system
  (lambda (x)
    (syntax-case x ()
      ((_ (name (param-name aspect-name ...)) body0 body ...)
       (syntax
        (define (name)
          (for-all-entities (lambda (param-name)
                              body0 body ...)
                            aspect-name ...)))))))


(define-syntax define-event
  (syntax-rules ()
    ((define-event (name . args)
       body0 body ...)
     (define (name . args)
       (register-event! 'name (lambda () body0 body ...))))))
