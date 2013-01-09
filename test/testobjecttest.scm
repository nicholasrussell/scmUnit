;;;; --------------------------------------------------------------------------
;;;; scmUnit
;;;;   Nicholas Russell
;;;; --------------------------------------------------------------------------
;;;; testobjecttest.scm
;;;;   testobject tests 
;;;; --------------------------------------------------------------------------

;; Assuming assert is tested thoroughly and can be used from now on

(let ((test-object (scmunit:testobject:test-object)))

  (assert-true (procedure? test-object))
  (assert-null (test-object 'get-name))
  (assert-null (scmunit:testobject:get-name test-object))
  (assert-null (test-object 'get-proc))
  (assert-null (scmunit:testobject:get-proc test-object))
  (assert-true (procedure? (test-object 'set-name)))
  (assert-true (procedure? (test-object 'set-proc)))
  ((test-object 'set-name) "abcd")
  (assert-equal "abcd" (test-object 'get-name))
  (assert-equal "abcd" (scmunit:testobject:get-name test-object))
  ((test-object 'set-proc) (lambda () #t))
  (assert-true (procedure? (test-object 'get-proc)))
  (assert-true (procedure? (scmunit:testobject:get-proc test-object)))
  (assert-true ((test-object 'get-proc)))
  (assert-true ((scmunit:testobject:get-proc test-object)))

  (assert-true (procedure? (scmunit:testobject:set-name test-object)))
  (assert-true (procedure? (scmunit:testobject:set-proc test-object)))
  ((scmunit:testobject:set-name test-object) "efgh")
  (assert-equal "efgh" (scmunit:testobject:get-name test-object))
  ((scmunit:testobject:set-proc test-object) (lambda () #f))
  (assert-true (procedure? (scmunit:testobject:get-proc test-object)))
  (assert-false ((scmunit:testobject:get-proc test-object)))

)

(let ((test-object (scmunit:testobject:create-test-object "test" (lambda () (+ 2 3) (+ 4 5)))))

  (assert-equal "test" (scmunit:testobject:get-name test-object))
  (assert-true (procedure? (scmunit:testobject:get-proc test-object)))
  (assert-= 9 ((scmunit:testobject:get-proc test-object)))

)