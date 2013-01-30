;;;; --------------------------------------------------------------------------
;;;; scmUnit
;;;;   Nicholas Russell
;;;; --------------------------------------------------------------------------
;;;; constants.scm
;;;;   Constants
;;;; --------------------------------------------------------------------------

(define scmunit:ok 'ok)
(define scmunit:done 'done)

(define scmunit:assertion-message "Assertion failed. Expected <~A>, was <~A>.")

(define scmunit:test-pass 'pass)
(define scmunit:test-fail 'fail)
(define scmunit:test-error 'error)

(define scmunit:test:testobject:test-object-type 'test-object)
(define scmunit:test:testresultobject:test-result-object-type 'test-result-object)
(define scmunit:testsuite:testsuiteobject:test-suite-object-type 'test-suite-object)
(define scmunit:testsuite:testsuitelist:test-suite-list-type 'test-suite-list)
(define scmunit:testfixture:testfixtureobject:test-fixture-object-type 'test-fixture-object)

(define scmunit:testfixture:types:before-suite 'before-suite)
(define scmunit:testfixture:types:after-suite 'after-suite)
(define scmunit:testfixture:types:before-test 'before-test)
(define scmunit:testfixture:types:after-test 'after-test)

(define scmunit:testsuite:default-test-suite-name "scmunit:default-test-suite")

(define scmunit:runlistener:before-suite 'before-suite)
(define scmunit:runlistener:after-suite 'after-suite)
(define scmunit:runlistener:before-test 'before-test)
(define scmunit:runlistener:after-test 'after-test)