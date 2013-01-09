;;;; --------------------------------------------------------------------------
;;;; scmUnit
;;;;   Nicholas Russell
;;;; --------------------------------------------------------------------------
;;;; test.scm ; rename definitions? Place define-test, define-suite, etc, here
;;;;   Define tests
;;;; --------------------------------------------------------------------------

;;;;
;; test
;;  Defines a test
;;
;; @param name
;; @param exp ...
(define-syntax define-test
  (syntax-rules ()
    ((define-test name exp ...)
      (let ((body (lambda () exp ...)))
        (add-test-object (scmunit:testobject:create-test-object name body))))))