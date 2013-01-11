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
;; run-test
;;  Runs a test by name `test-name'
;;
;; @param test-name
;; @return evaluated test result
(define (run-test test-name)
  (let* ((test-result (call-capture-errors (get-test-by-name test-name)))
         (test-eval (scmunit:evaluate-test-result test-result)))
    (set! test-results (append test-results (list (cons test-name test-eval))))
    test-eval))

(define (run-test-with-errors test-name)
  ((get-test-by-name test-name)))

(define (scmunit:test-passed? result)
  (eq? result scmunit:test-pass))

(define (scmunit:test-failed? result)
  (eq? result scmunit:test-fail))

(define (scmunit:test-error? result)
  (eq? result scmunit:test-error))
