;;;; --------------------------------------------------------------------------
;;;; scmUnit
;;;;   Nicholas Russell
;;;; --------------------------------------------------------------------------
;;;; testobjecttest.scm
;;;;   testobject tests 
;;;; --------------------------------------------------------------------------

;; Assuming assert is tested thoroughly and can be used from now on

(let ((test-object (scmunit:test:testobject:test-object)))

  (assert-true (procedure? test-object))
  (assert-null (test-object 'get-name))
  (assert-null (scmunit:test:testobject:get-name test-object))
  (assert-null (test-object 'get-proc))
  (assert-null (scmunit:test:testobject:get-proc test-object))
  (assert-true (procedure? (test-object 'set-name)))
  (assert-true (procedure? (test-object 'set-proc)))
  ((test-object 'set-name) "abcd")
  (assert-equal "abcd" (test-object 'get-name))
  (assert-equal "abcd" (scmunit:test:testobject:get-name test-object))
  ((test-object 'set-proc) (lambda () #t))
  (assert-true (procedure? (test-object 'get-proc)))
  (assert-true (procedure? (scmunit:test:testobject:get-proc test-object)))
  (assert-true ((test-object 'get-proc)))
  (assert-true ((scmunit:test:testobject:get-proc test-object)))

  (scmunit:test:testobject:set-name test-object "efgh")
  (assert-equal "efgh" (scmunit:test:testobject:get-name test-object))
  (scmunit:test:testobject:set-proc test-object (lambda () #f))
  (assert-true (procedure? (scmunit:test:testobject:get-proc test-object)))
  (assert-false ((scmunit:test:testobject:get-proc test-object)))

  (assert-true (scmunit:test:testobject:test-object? test-object))
  (assert-false (scmunit:test:testobject:test-object? '()))
  (assert-false (scmunit:test:testobject:test-object? (lambda () #t)))
  (assert-false (scmunit:test:testobject:test-object? (lambda (x) x)))
)

(let ((test-object (scmunit:test:testobject:create-test-object "test" (lambda () (+ 2 3) (+ 4 5)))))

  (assert-equal "test" (scmunit:test:testobject:get-name test-object))
  (assert-true (procedure? (scmunit:test:testobject:get-proc test-object)))
  (assert-= 9 ((scmunit:test:testobject:get-proc test-object)))

)
