;;;; --------------------------------------------------------------------------
;;;; scmUnit
;;;;   Nicholas Russell
;;;; --------------------------------------------------------------------------
;;;; testsuite.scm
;;;;   Define test suites
;;;; --------------------------------------------------------------------------

;;;;
;; scmunit:get-test-suites
;; 
;; @return test suites
(define (scmunit:get-test-suites)
  scmunit:*test-suites*)

;;;;
;; scmunit:set-test-suites
;;
;; @param test-suites
;; @return new test suites
(define (scmunit:set-test-suites test-suites)
  (when (not (list? test-suites))
    (error "Test suites not a list."))
  (set! scmunit:*test-suites* test-suites)
  (scmunit:get-test-suites))

;;;;
;; scmunit:get-current-test-suite
;;
;; @return current test suite
(define (scmunit:get-current-test-suite)
  scmunit:*current-test-suite*)

;;;;
;; scmunit:set-current-test-suite
;;
;; @param test suite name
(define (scmunit:set-current-test-suite suite-name)
  (when (not (member suite-name (map scmunit:testsuiteobject:get-name (scmunit:get-test-suites))))
    (error (string-append "Test suite " (string-append suite-name (string-append " is not defined. Define it first with define-test-suite.")))))
  (set! scmunit:*current-test-suite* suite-name)
  (scmunit:get-current-test-suite))

;;;;
;; scmunit:add-test-suite
;;
;; @param test-suite-object
(define (scmunit:add-test-suite test-suite-object)
  (when (not (scmunit:testsuiteobject:test-suite-object? test-suite-object))
    (error "Cannot add test suite, it is not a test suite object."))
  (scmunit:set-test-suites (append (scmunit:get-test-suites) (list test-suite-object))))

;;;;
;; define-test-suite
;;  Defines a test suite (a group of tests)
;;
;; @param suite-name - String
(define-syntax define-test-suite
  (syntax-rules ()
    ((define-test-suite suite-name)
      (let ((name (if (string? suite-name)
                        suite-name
                        (error "Test suite name must be a string."))))
        (let loop ((test-suites (map scmunit:testsuiteobject:get-name (scmunit:get-test-suites))))
          (if (null? test-suites)
            (begin
              (scmunit:add-test-suite (scmunit:testsuiteobject:create-test-suite-object name))
              scmunit:ok)
            (if (equal? (car test-suites) name)
              scmunit:ok
              (loop (cdr test-suites)))))))))

;;;;
;; begin-test-suite
;;  Begins defining a test suite.
;;  Any tests defined will be in the test suite `suite-name' until
;;  end-test-suite is called.
;;
;; @param suite-name - String
(define (begin-test-suite suite-name)
  (let ((name (if (string? suite-name)
                suite-name
                (error "Test suite name must be a symbol."))))
    (when (not (equal? scmunit:current-test-suite scmunit:default-test-suite-name))
      (error "Already in a test suite!"))
    (let loop ((test-suites (map scmunit:testsuiteobject:get-name (scmunit:get-test-suites))))
      (if (null? test-suites)
        (error "Test suite not found! Use define-test-suite to create it first.")
        (if (equal? (car test-suites) name)
          (begin 
            (scmunit:set-current-test-suite name)
            scmunit:ok)
          (loop (cdr test-suites)))))))

;;;;
;; end-test-suite
;;  Ends definition of a test suite.
(define (end-test-suite)
  (begin
    (scmunit:set-current-test-suite scmunit:default-test-suite-name)
    scmunit:ok))

;;;;
;; in-test-suite
;;  TODO think about this
;; 
;; @param name
(define-syntax in-test-suite
  (syntax-rules ()
    ((in-test-suite name tests ...)
      (let ((body (lambda () tests ...)))
        (begin-test-suite name)
        (body)
        (end-test-suite)))))

;; Set up default test suite
(define-test-suite scmunit:default-test-suite-name)
(scmunit:set-current-test-suite scmunit:default-test-suite-name)
