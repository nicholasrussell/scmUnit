;;;; --------------------------------------------------------------------------
;;;; scmUnit
;;;;   Nicholas Russell
;;;; --------------------------------------------------------------------------
;;;; test.scm
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
          (let ((test-suite-object (scmunit:testsuite:get-current-test-suite)))
            (when (null? test-suite-object)
              (error "Could not add test to test suite because test suite was not found."))
            (scmunit:testsuite:testsuiteobject:add-test test-suite-object (scmunit:test:testobject:create-test-object test-name body))
            (scmunit:testsuite:testsuitelist:update-test-suite (scmunit:testsuite:get-test-suite-list) test-suite-object)
            scmunit:ok))))))
