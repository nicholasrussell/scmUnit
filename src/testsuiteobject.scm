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
    (define (get-fixtures) fixtures)
    (define (get-test test-name)
      (let loop ((tests (get-tests)))
        (if (null? tests)
          '()
          (if (equal? (scmunit:test:testobject:get-name (car tests)) test-name)
            (car tests)
            (loop (cdr tests))))))
    (define (set-name n) (when (not (string? n)) (error "Test suite name must be a string.")) (set! name n) (get-name))
    (define (set-tests tsts) (set! tests tsts) (get-tests))
    (define (set-fixtures fxtrs) (set! fixtures fxtrs) (get-fixtures))
    (define (add-test test-object)
      (when (not (scmunit:test:testobject:test-object? test-object))
        (error "Cannot add non test object to test objects of test suite."))
      (set-tests (append (get-tests) (list test-object))))
    (define (add-fixture fixture-object)
      (when (not (scmunit:testfixture:testfixtureobject:test-fixture-object? fixture-object))
        (error "Cannot add non test fixture object to test fixtures of test suite."))
      (set-fixtures (append (get-fixtures) (list fixture-object))))

    (define (test-suite-object msg)
      (cond ((eq? msg 'get-name) (get-name))
            ((eq? msg 'get-tests) (get-tests))
            ((eq? msg 'get-fixtures) (get-fixtures))
            ((eq? msg 'get-test) (lambda (test-name) (get-test test-name)))
            ((eq? msg 'set-name) (lambda (n) (set-name n)))
            ((eq? msg 'add-test) (lambda (test-obj) (add-test test-obj)))
            ((eq? msg 'add-fixture) (lambda (fixture-obj) (add-fixture fixture-obj)))
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
;; scmunit:testsuite:testsuiteobject:get-fixtures
;;  Gets the tests of a test suite object
;;
;; @param test-suite-object
;; @return tests
(define (scmunit:testsuite:testsuiteobject:get-fixtures test-suite-object)
  (test-suite-object 'get-fixtures))

;;;;
;; scmunit:testsuite:testsuiteobject:get-before-suites test-suite-object
;;  Gets the before-suite fixtures
;;
;; @param test-suite-object
;; @return fixtures
(define (scmunit:testsuite:testsuiteobject:get-before-suites test-suite-object)
  (filter (lambda (fixture) (eq? (scmunit:testfixture:testfixtureobject:get-type fixture) scmunit:testfixture:types:before-suite)) (scmunit:testsuite:testsuiteobject:get-fixtures test-suite-object)))

;;;;
;; scmunit:testsuite:testsuiteobject:get-after-suites test-suite-object
;;  Gets the after-suite fixtures
;;
;; @param test-suite-object
;; @return fixtures
(define (scmunit:testsuite:testsuiteobject:get-after-suites test-suite-object)
  (filter (lambda (fixture) (eq? (scmunit:testfixture:testfixtureobject:get-type fixture) scmunit:testfixture:types:after-suite)) (scmunit:testsuite:testsuiteobject:get-fixtures test-suite-object)))

;;;;
;; scmunit:testsuite:testsuiteobject:get-before-tests test-suite-object
;;  Gets the before-test fixtures
;;
;; @param test-suite-object
;; @return fixtures
(define (scmunit:testsuite:testsuiteobject:get-before-tests test-suite-object)
  (filter (lambda (fixture) (eq? (scmunit:testfixture:testfixtureobject:get-type fixture) scmunit:testfixture:types:before-test)) (scmunit:testsuite:testsuiteobject:get-fixtures test-suite-object)))

;;;;
;; scmunit:testsuite:testsuiteobject:get-after-tests test-suite-object
;;  Gets the after-test fixtures
;;
;; @param test-suite-object
;; @return fixtures
(define (scmunit:testsuite:testsuiteobject:get-after-tests test-suite-object)
  (filter (lambda (fixture) (eq? (scmunit:testfixture:testfixtureobject:get-type fixture) scmunit:testfixture:types:after-test)) (scmunit:testsuite:testsuiteobject:get-fixtures test-suite-object)))

;;;;
;; scmunit:testsuite:testsuiteobject:get-test
;;  Gets a test from test suite object's tests
;;
;; @param test-suite-object
;; @param test-name
;; @return test object
(define (scmunit:testsuite:testsuiteobject:get-test test-suite-object test-name)
  ((test-suite-object 'get-test) test-name))

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
;; scmunit:testsuite:testsuiteobject:add-fixture
;;  Adds test-obj to test suite object
;;
;; @param test-suite-object
;; @param test-obj
(define (scmunit:testsuite:testsuiteobject:add-fixture test-suite-object fixture-obj)
  ((test-suite-object 'add-fixture) fixture-obj))

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
  (scmunit:check-object-type object scmunit:testsuite:testsuiteobject:test-suite-object-type))
