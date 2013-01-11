;;;; --------------------------------------------------------------------------
;;;; scmUnit
;;;;   Nicholas Russell
;;;; --------------------------------------------------------------------------
;;;; definetest.scm
;;;;   define-test tests 
;;;; --------------------------------------------------------------------------

(define-test "x = 2" (assert-equals 2 2 "x was not 2") (assert-equals 2 3 "x was not 2"))
(define-test "some-test" (assert-null '()))
