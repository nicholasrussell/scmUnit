;;;; --------------------------------------------------------------------------
;;;; scmUnit
;;;;   Nicholas Russell
;;;; --------------------------------------------------------------------------
;;;; testsuiteobjecttest.scm
;;;;   testsuiteobject tests 
;;;; --------------------------------------------------------------------------

;; Assuming assert is tested thoroughly and can be used from now on

(let ((test-suite-object (scmunit:testsuite:testsuiteobject:test-suite-object)))

  (assert-true (procedure? test-suite-object))
  (assert-null (test-suite-object 'get-name))
  (assert-null (scmunit:testsuite:testsuiteobject:get-name test-suite-object))
  (assert-null (test-suite-object 'get-tests))
  (assert-null (scmunit:testsuite:testsuiteobject:get-tests test-suite-object))
  (assert-true (procedure? (test-suite-object 'set-name)))
  (assert-true (procedure? (test-suite-object 'add-test)))
  ((test-suite-object 'set-name) "abcd")
  (assert-equal "abcd" (test-suite-object 'get-name))
  (assert-equal "abcd" (scmunit:testsuite:testsuiteobject:get-name test-suite-object))

  (scmunit:testsuite:testsuiteobject:set-name test-suite-object "efgh")
  (assert-equal "efgh" (scmunit:testsuite:testsuiteobject:get-name test-suite-object))

  (assert-true (scmunit:testsuite:testsuiteobject:test-suite-object? test-suite-object))
  (assert-false (scmunit:testsuite:testsuiteobject:test-suite-object? '()))
  (assert-false (scmunit:testsuite:testsuiteobject:test-suite-object? (lambda () #t)))
  (assert-false (scmunit:testsuite:testsuiteobject:test-suite-object? (lambda (x) x)))
)
