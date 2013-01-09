;;;; --------------------------------------------------------------------------
;;;; scmUnit
;;;;   Nicholas Russell
;;;; --------------------------------------------------------------------------
;;;; conditionstest.scm
;;;;   conditions tests 
;;;; --------------------------------------------------------------------------

(define (conditionstest:assert exp msg)
  (if (not exp)
    (error msg)))

;; condition-type:test-failure tests
(conditionstest:assert (condition-type? condition-type:test-failure) "condition-type:test-failure condition test failed")

;; integration test?
(let ((result (call-capture-errors (lambda () (test-failure "msg")))))
  (conditionstest:assert (and (condition? result) (condition/test-failure? result)) "condition big test failed"))
(let ((result (call-capture-errors (lambda () (error "msg")))))
  (conditionstest:assert (and (condition? result) (not (condition/test-failure? result))) "condition big test not test failure failed"))