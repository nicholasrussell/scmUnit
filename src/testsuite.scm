;;;; --------------------------------------------------------------------------
;;;; scmUnit
;;;;   Nicholas Russell
;;;; --------------------------------------------------------------------------
;;;; testsuite.scm
;;;;   Define test suites
;;;; --------------------------------------------------------------------------

;;;;
;; scmunit:testsuite:get-test-suite-list
;;
;; @return test suite list
(define (scmunit:testsuite:get-test-suite-list)
  scmunit:testsuite:*test-suites*)

;;;;
;; scmunit:testsuite:get-test-suites
;; 
;; @return test suites
(define (scmunit:testsuite:get-test-suites)
  (scmunit:testsuite:testsuitelist:get-test-suites (scmunit:testsuite:get-test-suite-list)))

;;;;
;; scmunit:testsuite:set-test-suites
;;
;; @param test-suites
;; @return new test suites
(define (scmunit:testsuite:set-test-suites test-suites)
  (when (not (scmunit:testsuite:testsuitelist:test-suite-list? test-suites))
    (error "Test suites not a test suite list."))
  (set! scmunit:testsuite:*test-suites* test-suites)
  (scmunit:testsuite:get-test-suites))

;;;;
;; scmunit:testsuite:get-current-test-suite-name
;;
;; @return current test suite name
(define (scmunit:testsuite:get-current-test-suite-name)
  scmunit:testsuite:*current-test-suite-name*)

;;;;
;; scmunit:testsuite:get-current-test-suite
;;
;; @return current test suite
(define (scmunit:testsuite:get-current-test-suite)
  (scmunit:testsuite:testsuitelist:get-test-suite (scmunit:testsuite:get-test-suite-list) (scmunit:testsuite:get-current-test-suite-name)))

;;;;
;; scmunit:testsuite:set-current-test-suite-name
;;
;; @param test suite name
(define (scmunit:testsuite:set-current-test-suite-name suite-name)
  (when (not (member suite-name (map scmunit:testsuite:testsuiteobject:get-name (scmunit:testsuite:get-test-suites))))
    (error (string-append "Test suite " (string-append suite-name (string-append " is not defined. Define it first with define-test-suite.")))))
  (set! scmunit:testsuite:*current-test-suite-name* suite-name)
  (scmunit:testsuite:get-current-test-suite-name))

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
        (let loop ((test-suites (map scmunit:testsuite:testsuiteobject:get-name (scmunit:testsuite:get-test-suites))))
          (if (null? test-suites)
            (begin
              (scmunit:testsuite:testsuitelist:add-test-suite (scmunit:testsuite:get-test-suite-list) (scmunit:testsuite:testsuiteobject:create-test-suite-object name))
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
    (when (not (equal? scmunit:testsuite:current-test-suite scmunit:testsuite:default-test-suite-name))
      (error "Already in a test suite!"))
    (let loop ((test-suites (map scmunit:testsuite:testsuiteobject:get-name (scmunit:testsuite:get-test-suites))))
      (if (null? test-suites)
        (error "Test suite not found! Use define-test-suite to create it first.")
        (if (equal? (car test-suites) name)
          (begin 
            (scmunit:testsuite:set-current-test-suite-name name)
            scmunit:ok)
          (loop (cdr test-suites)))))))

;;;;
;; end-test-suite
;;  Ends definition of a test suite.
(define (end-test-suite)
  (begin
    (scmunit:testsuite:set-current-test-suite-name scmunit:testsuite:default-test-suite-name)
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
        (end-test-suite)
        (scmunit:ok)))))

;; Set up default test suite
(scmunit:testsuite:set-test-suites (scmunit:testsuite:testsuitelist:create-test-suite-list))
(define-test-suite scmunit:testsuite:default-test-suite-name)
(scmunit:testsuite:set-current-test-suite-name scmunit:testsuite:default-test-suite-name)
