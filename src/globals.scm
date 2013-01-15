;;;; --------------------------------------------------------------------------
;;;; scmUnit
;;;;   Nicholas Russell
;;;; --------------------------------------------------------------------------
;;;; globals.scm
;;;;   Globals
;;;; --------------------------------------------------------------------------

(define scmunit:testsuite:*test-suites* '())
(define scmunit:testsuite:*current-test-suite-name* '())

(define scmunit:*test-run-results* '())

(define scmunit:runlistener:*run-listener* '())