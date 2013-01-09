;;;; --------------------------------------------------------------------------
;;;; scmUnit
;;;;   Nicholas Russell
;;;; --------------------------------------------------------------------------
;;;; asserttest.scm
;;;;   assert tests 
;;;; --------------------------------------------------------------------------

;; TODO Block output

(define (asserttest:assert exp msg)
  (if (not exp)
    (error msg)))

;; scmunit:assert-exp tests
(let ((result (call-capture-errors (lambda () (scmunit:assert-exp (lambda () #t) 1 1 '())))))
  (asserttest:assert (not (condition/test-failure? result)) "scmunit:assert-exp #t failed"))
(let ((result (call-capture-errors (lambda () (scmunit:assert-exp (lambda () #f) 1 1 '())))))
  (asserttest:assert (condition/test-failure? result) "scmunit:assert-exp #f failed"))

;; assert-null tests
(let ((result (call-capture-errors (lambda () (assert-null '())))))
  (asserttest:assert (not (condition/test-failure? result)) "assert-null #t failed"))
(let ((result (call-capture-errors (lambda () (assert-null (list 1 2 3))))))
  (asserttest:assert (condition/test-failure? result) "assert-null #f failed"))

;; assert-not-null tests
(let ((result (call-capture-errors (lambda () (assert-not-null (list 1 2 3))))))
  (asserttest:assert (not (condition/test-failure? result)) "assert-not-null #t failed"))
(let ((result (call-capture-errors (lambda () (assert-not-null '())))))
  (asserttest:assert (condition/test-failure? result) "assert-not-null #f failed"))

;; assert-true tests
(let ((result (call-capture-errors (lambda () (assert-true #t)))))
  (asserttest:assert (not (condition/test-failure? result)) "assert-true #t failed"))
(let ((result (call-capture-errors (lambda () (assert-true #f)))))
  (asserttest:assert (condition/test-failure? result) "assert-true #f failed"))

;; assert-false tests
(let ((result (call-capture-errors (lambda () (assert-false #f)))))
  (asserttest:assert (not (condition/test-failure? result)) "assert-false #t failed"))
(let ((result (call-capture-errors (lambda () (assert-false #t)))))
  (asserttest:assert (condition/test-failure? result) "assert-false #f failed"))

;; assert-equal tests
(let ((result (call-capture-errors (lambda () (assert-equal #t #t)))))
  (asserttest:assert (not (condition/test-failure? result)) "assert-equal #t failed"))
(let ((result (call-capture-errors (lambda () (assert-equal #f #t)))))
  (asserttest:assert (condition/test-failure? result) "assert-equal #f failed"))

;; assert-eq tests
(let ((result (call-capture-errors (lambda () (assert-eq 'a 'a)))))
  (asserttest:assert (not (condition/test-failure? result)) "assert-eq #t failed"))
(let ((result (call-capture-errors (lambda () (assert-eq 'a 'b)))))
  (asserttest:assert (condition/test-failure? result) "assert-eq #f failed"))

;; assert-eqv tests
(let ((lst (list 1 2 3)))
  (let ((result (call-capture-errors (lambda () (assert-eqv lst lst)))))
    (asserttest:assert (not (condition/test-failure? result)) "assert-eqv #t failed"))
  (let ((result (call-capture-errors (lambda () (assert-eqv lst (list 1 2 3))))))
    (asserttest:assert (condition/test-failure? result) "assert-eqv #f failed"))
)

;; assert-= tests
(let ((result (call-capture-errors (lambda () (assert-= 1 1)))))
  (asserttest:assert (not (condition/test-failure? result)) "assert-= #t failed"))
(let ((result (call-capture-errors (lambda () (assert-= 1 2)))))
  (asserttest:assert (condition/test-failure? result) "assert-= #f failed"))

;; assert-fail tests
(let ((result (call-capture-errors (lambda () (assert-fail)))))
  (asserttest:assert (condition/test-failure? result) "assert-fail test failed"))
