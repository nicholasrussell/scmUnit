;;;; --------------------------------------------------------------------------
;;;; scmUnit
;;;;   Nicholas Russell
;;;; --------------------------------------------------------------------------
;;;; runnertest.scm
;;;;   runner tests 
;;;; --------------------------------------------------------------------------

(let ((failure (call-capture-errors (lambda () (assertion-failure "failure msg"))))
      (err (call-capture-errors (lambda () (error "error msg")))))

  (assert-eq 'fail (scmunit:evaluate-test-result failure))
  (assert-eq 'error (scmunit:evaluate-test-result err))
  (assert-eq 'pass (scmunit:evaluate-test-result #t))
  (assert-eq 'pass (scmunit:evaluate-test-result '()))

)