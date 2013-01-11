;;;; --------------------------------------------------------------------------
;;;; scmUnit
;;;;   Nicholas Russell
;;;; --------------------------------------------------------------------------
;;;; testsuiteobject.scm
;;;;   Test suite object
;;;; --------------------------------------------------------------------------

;; test-suite-object
(define (scmunit:testsuiteobject:test-suite-object)
  (let ((name '())
        (fixtures '())
        (tests '()))

    (define (get-name) name)
    (define (get-tests) tests)
    (define (set-name n) (begin (when (not (string? n)) (error "Test suite name must be a string.")) (set! name n) name))
    (define (set-tests tsts) (begin (set! tests tsts) tests))
    (define (add-test test-object)
      (set-tests (append (get-tests) (list test-object)))) ;; assert test-object? ?

    (define (test-suite-object msg)
      (cond ((eq? msg 'get-name) (get-name))
            ((eq? msg 'get-tests) (get-tests))
            ((eq? msg 'set-name) (lambda (n) (set-name n)))
            ;((eq? msg 'set-tests) (lambda (tsts) (set-tests tests))) ;; necessary to expose?
            ((eq? msg 'add-test) (lambda (test-obj) (add-test test-obj)))
            ((eq? msg 'type) scmunit:testsuiteobject:test-suite-object-type)))

    test-suite-object))

;;;;
;; scmunit:testsuiteobject:get-name
;;  Gets the name of a test suite object
;;
;; @param test-suite-object
;; @return name
(define (scmunit:testsuiteobject:get-name test-suite-object)
  (test-suite-object 'get-name))

;;;;
;; scmunit:testsuiteobject:get-tests
;;  Gets the tests of a test suite object
;;
;; @param test-suite-object
;; @return tests
(define (scmunit:testsuiteobject:get-tests test-suite-object)
  (test-suite-object 'get-tests))

;;;;
;; scmunit:testsuiteobject:set-name
;;  Sets the name of a test suite
;;
;; @param test-suite-object
;; @return procedure
(define (scmunit:testsuiteobject:set-name test-suite-object)
  (lambda (n) ((test-suite-object 'set-name) n)))

;;;;
;; scmunit:testsuiteobject:add-test
;;  Adds test-obj to test suite object
;;
;; @param test-suite-object
;; @return procedure
(define (scmunit:testsuiteobject:add-test test-suite-object)
  (lambda (test-obj) ((test-suite-object 'add-test) test-obj)))

;;;;
;; scmunit:testsuiteobject:create-test-suite-object
;;  Creates a test suite object
;;
;; @param name
;; @return test suite object
(define (scmunit:testsuiteobject:create-test-suite-object name)
  (when (not (string? name))
    (error "Test suite name must be a string."))
  (let ((test-suite-obj (scmunit:testsuiteobject:test-suite-object)))
    ((scmunit:testsuiteobject:set-name test-suite-obj) name)

    test-suite-obj))

;;;;
;; scmunit:testsuiteobject:test-suite-object?
;;  Returns true if object is a test-suite-object
;;
;; @param object
;; @return boolean
(define (scmunit:testsuiteobject:test-suite-object? object)
  (if (or (null? object) (not (procedure? object)))
    #f
    (let ((arity (procedure-arity object)))
      (if (or (not (= (car arity) 1)) (not (= (cdr arity) 1)))
        #f
        (let ((result (call-capture-errors (lambda () (object 'type)))))
          (if (condition? result)
            #f
            (eq? result scmunit:testsuiteobject:test-suite-object-type)))))))
