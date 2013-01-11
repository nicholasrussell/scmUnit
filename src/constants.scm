;;;; --------------------------------------------------------------------------
;;;; scmUnit
;;;;   Nicholas Russell
;;;; --------------------------------------------------------------------------
;;;; constants.scm
;;;;   Constants
;;;; --------------------------------------------------------------------------

(define scmunit:ok 'ok)

(define scmunit:assertion-message "Assertion failed. Expected <~A>, was <~A>.")

(define scmunit:test-pass 'pass)
(define scmunit:test-fail 'fail)
(define scmunit:test-error 'error)

(define scmunit:testobject:test-object-type 'test-object)
(define scmunit:testsuiteobject:test-suite-object-type 'test-suite-object)

(define scmunit:default-test-suite-name "scmunit:default-test-suite")
(define scmunit:*test-suites* '())
(define scmunit:*current-test-suite* '())
