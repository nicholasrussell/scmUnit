;;;; --------------------------------------------------------------------------
;;;; scmUnit
;;;;   Nicholas Russell
;;;; --------------------------------------------------------------------------
;;;; testsuiteobject.scm
;;;;   Test suite object
;;;; --------------------------------------------------------------------------

;; test-suite-object
(define (scmunit:testsuite:testsuiteobject:test-suite-object)
  (let ((name '())
        (fixtures '())
        (tests '()))

    (define (get-name) name)
    (define (get-tests) tests)
    (define (set-name n) (when (not (string? n)) (error "Test suite name must be a string.")) (set! name n) (get-name))
    (define (set-tests tsts) (set! tests tsts) (get-tests))
    (define (add-test test-object)
      (when (not (scmunit:test:testobject:test-object? test-object))
        (error "Cannot add non test object to test objects of test suite"))
      (set-tests (append (get-tests) (list test-object)))) ;; assert test-object? ?

    (define (test-suite-object msg)
      (cond ((eq? msg 'get-name) (get-name))
            ((eq? msg 'get-tests) (get-tests))
            ((eq? msg 'set-name) (lambda (n) (set-name n)))
            ((eq? msg 'add-test) (lambda (test-obj) (add-test test-obj)))
            ((eq? msg 'type) scmunit:testsuite:testsuiteobject:test-suite-object-type)))

    test-suite-object))

;;;;
;; scmunit:testsuite:testsuiteobject:get-name
;;  Gets the name of a test suite object
;;
;; @param test-suite-object
;; @return name
(define (scmunit:testsuite:testsuiteobject:get-name test-suite-object)
  (test-suite-object 'get-name))

;;;;
;; scmunit:testsuite:testsuiteobject:get-tests
;;  Gets the tests of a test suite object
;;
;; @param test-suite-object
;; @return tests
(define (scmunit:testsuite:testsuiteobject:get-tests test-suite-object)
  (test-suite-object 'get-tests))

;;;;
;; scmunit:testsuite:testsuiteobject:set-name
;;  Sets the name of a test suite
;;
;; @param test-suite-object
;; @param test-name
(define (scmunit:testsuite:testsuiteobject:set-name test-suite-object test-name)
  ((test-suite-object 'set-name) test-name))

;;;;
;; scmunit:testsuite:testsuiteobject:add-test
;;  Adds test-obj to test suite object
;;
;; @param test-suite-object
;; @param test-obj
(define (scmunit:testsuite:testsuiteobject:add-test test-suite-object test-obj)
  ((test-suite-object 'add-test) test-obj))

;;;;
;; scmunit:testsuite:testsuiteobject:create-test-suite-object
;;  Creates a test suite object
;;
;; @param name
;; @return test suite object
(define (scmunit:testsuite:testsuiteobject:create-test-suite-object name)
  (when (not (string? name))
    (error "Test suite name must be a string."))
  (let ((test-suite-obj (scmunit:testsuite:testsuiteobject:test-suite-object)))
    (scmunit:testsuite:testsuiteobject:set-name test-suite-obj name)

    test-suite-obj))

;;;;
;; scmunit:testsuite:testsuiteobject:test-suite-object?
;;  Returns true if object is a test-suite-object
;;
;; @param object
;; @return boolean
(define (scmunit:testsuite:testsuiteobject:test-suite-object? object)
  (if (or (null? object) (not (procedure? object)))
    #f
    (let ((arity (procedure-arity object)))
      (if (or (not (= (car arity) 1)) (not (= (cdr arity) 1)))
        #f
        (let ((result (call-capture-errors (lambda () (object 'type)))))
          (if (condition? result)
            #f
            (eq? result scmunit:testsuite:testsuiteobject:test-suite-object-type)))))))
