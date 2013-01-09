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

;; condition-type:assertion-failure tests
(conditionstest:assert (condition-type? condition-type:assertion-failure) "condition-type:assertion-failure condition test failed")

;; integration test?
(let ((result (call-capture-errors (lambda () (assertion-failure "msg")))))
  (conditionstest:assert (and (condition? result) (condition/assertion-failure? result)) "condition big test failed"))
(let ((result (call-capture-errors (lambda () (error "msg")))))
  (conditionstest:assert (and (condition? result) (not (condition/assertion-failure? result))) "condition big test not test failure failed"))