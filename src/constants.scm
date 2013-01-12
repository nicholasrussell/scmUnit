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

(define scmunit:test:testobject:test-object-type 'test-object)
(define scmunit:testresultobject:test-result-object-type 'test-result-object)
(define scmunit:testsuite:testsuiteobject:test-suite-object-type 'test-suite-object)
(define scmunit:testsuite:testsuitelist:test-suite-list-type 'test-suite-list)

(define scmunit:*test-run-result* '())

(define scmunit:testsuite:default-test-suite-name "scmunit:default-test-suite")
(define scmunit:testsuite:*test-suites* '())
(define scmunit:testsuite:*current-test-suite-name* '())

