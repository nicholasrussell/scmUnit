;;;; --------------------------------------------------------------------------
;;;; scmUnit
;;;;   Nicholas Russell
;;;; --------------------------------------------------------------------------
;;;; runner.scm
;;;;   Test runner
;;;; --------------------------------------------------------------------------

;;;;
;; scmunit:evaluate-test-result
;;  Checks conditions to evaluate a test result
;;
;; @param test-result
;; @return result - 'fail, 'error, or 'pass
(define (scmunit:evaluate-test-result test-result)
  (cond
    ((and (condition? test-result)
          (condition/assertion-failure? test-result)) scmunit:test-fail)
    ((and (condition? test-result)
          (condition/error? test-result)) scmunit:test-error)
    (else scmunit:test-pass)))

;;;;
;; scmunit:run-test
;;  Runs a single test
;;
;; @param test-name
;; @param test-suite-name
;; @param clear-results
(define (scmunit:run-test test-name test-suite-name)
  (define test-suite '())
  (define test-object '())
  (define test '())
  (let ((test-suite-by-name (scmunit:testsuite:testsuitelist:get-test-suite (scmunit:testsuite:get-test-suite-list) test-suite-name)))
    (when (null? test-suite-by-name)
      (error (string-append "Could not find test suite by name " test-suite-name)))
    (set! test-suite test-suite-by-name)
    (set! test-object (scmunit:testsuite:testsuiteobject:get-test test-suite test-name)))
  (when (not (null? test-object))
    (set! test (scmunit:test:testobject:get-proc test-object)))
  (when (null? test)
    (error (string-append "Could not find test " (string-append test-name (string-append " in suite " test-suite-name)))))
  (scmunit:runlistener:notify-listener scmunit:runlistener:before-test test-name)
  (define test-result-object '())
  (let before-test-loop ((before-tests (scmunit:testsuite:testsuiteobject:get-before-tests test-suite)))
    (when (and (not (null? before-tests)) (null? test-result-object))
      (let ((fixture-eval (scmunit:evaluate-test-result (call-capture-errors (scmunit:testfixture:testfixtureobject:get-proc (car before-tests))))))
        (when (not (scmunit:test-passed? fixture-eval))
          (set! test-result-object (scmunit:test:testresultobject:create-test-result-object test-suite-name test-name (scmunit:evaluate-test-result fixture-eval)))))
      (before-test-loop (cdr before-tests))))
  (when (null? test-result-object)
    (set! test-result-object (scmunit:test:testresultobject:create-test-result-object test-suite-name test-name (scmunit:evaluate-test-result (call-capture-errors test)))))
  (let after-test-loop ((after-tests (scmunit:testsuite:testsuiteobject:get-after-tests test-suite)))
    (when (not (null? after-tests))
      (let ((fixture-eval (scmunit:evaluate-test-result (call-capture-errors (scmunit:testfixture:testfixtureobject:get-proc (car after-tests))))))
        (when (and (not (scmunit:test-passed? fixture-eval)) (null? test-result-object))
          (set! test-result-object (scmunit:test:testresultobject:create-test-result-object test-suite-name test-name (scmunit:evaluate-test-result fixture-eval)))))
      (after-test-loop (cdr after-tests))))
  (scmunit:runlistener:notify-listener scmunit:runlistener:after-test test-name test-result-object)
  (set! scmunit:*test-run-results* (append scmunit:*test-run-results* (list test-result-object)))
  (let ((test-results (list-copy scmunit:*test-run-results*)))
    test-results))

;;;;
;; run-test
;;  Runs a test by name `test-name'
;;
;; @param test-name
;; @return evaluated test result
(define (run-test test-name)
  (set! scmunit:*test-run-results* '())
  (define test-suite '())
  (let loop ((test-suites (scmunit:testsuite:get-test-suites)))
    (if (null? test-suites)
      '()
      (if (not (null? (scmunit:testsuite:testsuiteobject:get-test (car test-suites) test-name)))
        (set! test-suite (car test-suites))
        (loop (cdr test-suites)))))

  (when (null? test-suite)
    (error "Could not find test suite for test."))

  (scmunit:runlistener:notify-listener scmunit:runlistener:before-suite (scmunit:testsuite:testsuiteobject:get-name test-suite))
  (let ((test-results '()))
    (define fixture-result '())

    (let before-suite-loop ((before-suites (scmunit:testsuite:testsuiteobject:get-before-suites test-suite)))
      (when (and (not (null? before-suites)) (null? fixture-result))
        (let ((fixture-eval (scmunit:evaluate-test-result (call-capture-errors (scmunit:testfixture:testfixtureobject:get-proc (car before-suites))))))
          (when (not (scmunit:test-passed? fixture-eval))
            (set! fixture-result (scmunit:test:testresultobject:create-test-result-object test-suite-name test-name (scmunit:evaluate-test-result fixture-eval)))))
        (before-suite-loop (cdr before-suites))))

    (when (null? fixture-result)
      (set! test-results (append test-results (scmunit:run-test test-name (scmunit:testsuite:testsuiteobject:get-name test-suite)))))

    (let after-suite-loop ((after-suites (scmunit:testsuite:testsuiteobject:get-after-suites test-suite)))
      (when (and (not (null? after-suites)) (null? fixture-result))
        (let ((fixture-eval (scmunit:evaluate-test-result (call-capture-errors (scmunit:testfixture:testfixtureobject:get-proc (car after-suites))))))
          (when (not (scmunit:test-passed? fixture-eval))
            (set! fixture-result (scmunit:test:testresultobject:create-test-result-object test-suite-name test-name (scmunit:evaluate-test-result fixture-eval)))))
        (after-suite-loop (cdr after-suites))))

    (when (not (null? fixture-result))
      (set! test-results (list fixture-result)))
    (scmunit:runlistener:notify-listener scmunit:runlistener:after-suite (scmunit:testsuite:testsuiteobject:get-name test-suite))
    test-results))

;;;;
;; scmunit:run-test-suite
;;
;; @param test-suite-name
(define (scmunit:run-test-suite test-suite-name)
  (define test-suite (scmunit:testsuite:testsuitelist:get-test-suite (scmunit:testsuite:get-test-suite-list) test-suite-name))
  (when (null? test-suite)
    (error (string-append "Could not find test suite by name " test-suite-name)))
  (scmunit:runlistener:notify-listener scmunit:runlistener:before-suite test-suite-name)
  (let ((test-results '()))
    (define fixture-result '())

    (let before-suite-loop ((before-suites (scmunit:testsuite:testsuiteobject:get-before-suites test-suite)))
      (when (and (not (null? before-suites)) (null? fixture-result))
        (let ((fixture-eval (scmunit:evaluate-test-result (call-capture-errors (scmunit:testfixture:testfixtureobject:get-proc (car before-suites))))))
          (when (not (scmunit:test-passed? fixture-eval))
            (set! fixture-result (scmunit:test:testresultobject:create-test-result-object test-suite-name test-name (scmunit:evaluate-test-result fixture-eval)))))
        (before-suite-loop (cdr before-suites))))

    (when (null? fixture-result)
      (let loop ((suite-tests (scmunit:testsuite:testsuiteobject:get-tests test-suite)))
        (if (null? suite-tests)
          (set! test-results (list-copy scmunit:*test-run-results*))
          (begin
            (scmunit:run-test (scmunit:test:testobject:get-name (car suite-tests)) test-suite-name)
            (loop (cdr suite-tests))))))

    (let after-suite-loop ((after-suites (scmunit:testsuite:testsuiteobject:get-after-suites test-suite)))
      (when (and (not (null? after-suites)) (null? fixture-result))
        (let ((fixture-eval (scmunit:evaluate-test-result (call-capture-errors (scmunit:testfixture:testfixtureobject:get-proc (car after-suites))))))
          (when (not (scmunit:test-passed? fixture-eval))
            (set! fixture-result (scmunit:test:testresultobject:create-test-result-object test-suite-name test-name (scmunit:evaluate-test-result fixture-eval)))))
        (after-suite-loop (cdr after-suites))))

    (when (not (null? fixture-result))
      (set! test-results (list fixture-result)))
    (scmunit:runlistener:notify-listener scmunit:runlistener:after-suite test-suite-name)
    test-results))

;;;;
;; run-test-suite
;;  Runs all tests in suite `test-suite-name'
;;
;; @param test-suite-name
(define (run-test-suite test-suite-name)
  (set! scmunit:*test-run-results* '())
  (scmunit:run-test-suite test-suite-name))

;;;;
;; scmunit:run-tests
(define (scmunit:run-tests)
  (let loop ((test-suites (scmunit:testsuite:get-test-suites)))
    (if (null? test-suites)
      (let ((test-results (list-copy scmunit:*test-run-results*)))
        test-results)
      (begin
        (scmunit:run-test-suite (scmunit:testsuite:testsuiteobject:get-name (car test-suites)))
        (loop (cdr test-suites))))))

;;;;
;; run-tests
;;  Runs all tests
(define (run-tests)
  (set! scmunit:*test-run-results* '())
  (scmunit:run-tests))

;;;;
;; print-results
;;
;; @param results
(define (print-test-results results)
  (define (result-to-text result)
    (if (scmunit:test-passed? result)
      "Passed"
      (if (scmunit:test-failed? result)
        "Failed"
        (if (scmunit:test-error? result)
          "Error"
          "Unknown"))))
  (let loop ((test-results results)
             (passed 0))
    (if (null? test-results)
      (begin
        (format (current-output-port) "Passed: ~A/~A~%" passed (length results))
        scmunit:done)
      (begin
        (format (current-output-port)
                "Test name: ~A, Test suite: ~A~%Result: ~A~%"
                (scmunit:test:testresultobject:get-test-name (car test-results))
                (scmunit:test:testresultobject:get-test-suite-name (car test-results))
                (result-to-text (scmunit:test:testresultobject:get-test-result (car test-results))))
        (loop (cdr test-results)
              (if (scmunit:test-passed? (scmunit:test:testresultobject:get-test-result (car test-results)))
                (1+ passed)
                passed))))))

;;;;
;; print-results
(define (print-results)
  (print-test-results scmunit:*test-run-results*))

;; TODO Move these to test result object??
(define (scmunit:test-passed? result)
  (eq? result scmunit:test-pass))

(define (scmunit:test-failed? result)
  (eq? result scmunit:test-fail))

(define (scmunit:test-error? result)
  (eq? result scmunit:test-error))
