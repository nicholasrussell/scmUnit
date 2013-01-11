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
;; @param name - String
;; @param exp ...
(define-syntax define-test
  (syntax-rules ()
    ((define-test name exp ...)
      (let ((body (lambda () exp ...)))
        (let ((test-name (if (string? name)
                          name
                          (if (symbol? name)
                            (symbol->string name)
                            (error "Test name must be a string.")))))
          (add-test-object (scmunit:testobject:create-test-object name body))
          scmunit:ok)))))
