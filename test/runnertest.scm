;;;; --------------------------------------------------------------------------
;;;; scmUnit
;;;;   Nicholas Russell
;;;; --------------------------------------------------------------------------
;;;; runnertest.scm
;;;;   runner tests 
;;;; --------------------------------------------------------------------------

(assert-true (scmunit:test-passed? scmunit:test-pass))
(assert-false (scmunit:test-passed? scmunit:test-fail))
(assert-false (scmunit:test-passed? scmunit:test-error))
(assert-true (scmunit:test-failed? scmunit:test-fail))
(assert-false (scmunit:test-failed? scmunit:test-pass))
(assert-false (scmunit:test-failed? scmunit:test-error))
(assert-true (scmunit:test-error? scmunit:test-error))
(assert-false (scmunit:test-error? scmunit:test-pass))
(assert-false (scmunit:test-error? scmunit:test-fail))

(let ((failure (call-capture-errors (lambda () (assertion-failure "failure msg"))))
      (err (call-capture-errors (lambda () (error "error msg")))))

  (assert-eq 'fail (scmunit:evaluate-test-result failure))
  (assert-eq 'error (scmunit:evaluate-test-result err))
  (assert-eq 'pass (scmunit:evaluate-test-result #t))
  (assert-eq 'pass (scmunit:evaluate-test-result '()))

)
