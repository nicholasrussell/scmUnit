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
          (condition/assertion-failure? test-result)) 'fail)
    ((and (condition? test-result)
          (condition/error? test-result)) 'error)
    (else 'pass)))

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

(define (run-error test-name)
  ((get-test-by-name test-name)))