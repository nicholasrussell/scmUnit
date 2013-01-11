;;;; --------------------------------------------------------------------------
;;;; scmUnit
;;;;   Nicholas Russell
;;;; --------------------------------------------------------------------------
;;;; testsuiteobjecttest.scm
;;;;   testsuiteobject tests 
;;;; --------------------------------------------------------------------------

;; Assuming assert is tested thoroughly and can be used from now on

(let ((test-suite-object (scmunit:testsuiteobject:test-suite-object)))

  (assert-true (procedure? test-suite-object))
  (assert-null (test-suite-object 'get-name))
  (assert-null (scmunit:testsuiteobject:get-name test-suite-object))
  (assert-null (test-suite-object 'get-tests))
  (assert-null (scmunit:testsuiteobject:get-tests test-suite-object))
  (assert-true (procedure? (test-suite-object 'set-name)))
  (assert-true (procedure? (test-suite-object 'add-test)))
  ((test-suite-object 'set-name) "abcd")
  (assert-equal "abcd" (test-suite-object 'get-name))
  (assert-equal "abcd" (scmunit:testsuiteobject:get-name test-suite-object))

  (assert-true (procedure? (scmunit:testsuiteobject:set-name test-suite-object)))
  ((scmunit:testsuiteobject:set-name test-suite-object) "efgh")
  (assert-equal "efgh" (scmunit:testsuiteobject:get-name test-suite-object))

  (assert-true (scmunit:testsuiteobject:test-suite-object? test-suite-object))
  (assert-false (scmunit:testsuiteobject:test-suite-object? '()))
  (assert-false (scmunit:testsuiteobject:test-suite-object? (lambda () #t)))
  (assert-false (scmunit:testsuiteobject:test-suite-object? (lambda (x) x)))
)
