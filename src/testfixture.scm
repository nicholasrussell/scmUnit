;;;; --------------------------------------------------------------------------
;;;; scmUnit
;;;;   Nicholas Russell
;;;; --------------------------------------------------------------------------
;;;; testfixture.scm
;;;;   Define fixtures
;;;; --------------------------------------------------------------------------

;;;;
;; define-before-suite
;;  Defines a before-suite fixture
;;
;; @param exp ...
(define-syntax define-before-suite
  (syntax-rules ()
    ((define-before-suite exp ...)
      (let ((body (lambda () exp ...)))
        (let ((test-fixture-object (scmunit:testfixture:testfixtureobject:create-test-fixture-object scmunit:testfixture:types:before-suite body)))
          (let ((test-suite-object (scmunit:testsuite:get-current-test-suite)))
            (when (null? test-suite-object)
              (error "Could not add test fixture to test suite because test suite was not found."))
            (scmunit:testsuite:testsuiteobject:add-fixture test-suite-object test-fixture-object)
            (scmunit:testsuite:testsuitelist:update-test-suite (scmunit:testsuite:get-test-suite-list) test-suite-object)
            scmunit:ok))))))

;;;;
;; define-after-suite
;;  Defines a after-suite fixture
;;
;; @param exp ...
(define-syntax define-after-suite
  (syntax-rules ()
    ((define-after-suite exp ...)
      (let ((body (lambda () exp ...)))
        (let ((test-fixture-object (scmunit:testfixture:testfixtureobject:create-test-fixture-object scmunit:testfixture:types:after-suite body)))
          (let ((test-suite-object (scmunit:testsuite:get-current-test-suite)))
            (when (null? test-suite-object)
              (error "Could not add test fixture to test suite because test suite was not found."))
            (scmunit:testsuite:testsuiteobject:add-fixture test-suite-object test-fixture-object)
            (scmunit:testsuite:testsuitelist:update-test-suite (scmunit:testsuite:get-test-suite-list) test-suite-object)
            scmunit:ok))))))

;;;;
;; define-before-test
;;  Defines a before-test fixture
;;
;; @param exp ...
(define-syntax define-before-test
  (syntax-rules ()
    ((define-before-teste exp ...)
      (let ((body (lambda () exp ...)))
        (let ((test-fixture-object (scmunit:testfixture:testfixtureobject:create-test-fixture-object scmunit:testfixture:types:before-test body)))
          (let ((test-suite-object (scmunit:testsuite:get-current-test-suite)))
            (when (null? test-suite-object)
              (error "Could not add test fixture to test suite because test suite was not found."))
            (scmunit:testsuite:testsuiteobject:add-fixture test-suite-object test-fixture-object)
            (scmunit:testsuite:testsuitelist:update-test-suite (scmunit:testsuite:get-test-suite-list) test-suite-object)
            scmunit:ok))))))

;;;;
;; define-after-test
;;  Defines a after-test fixture
;;
;; @param exp ...
(define-syntax define-after-test
  (syntax-rules ()
    ((define-after-teste exp ...)
      (let ((body (lambda () exp ...)))
        (let ((test-fixture-object (scmunit:testfixture:testfixtureobject:create-test-fixture-object scmunit:testfixture:types:after-test body)))
          (let ((test-suite-object (scmunit:testsuite:get-current-test-suite)))
            (when (null? test-suite-object)
              (error "Could not add test fixture to test suite because test suite was not found."))
            (scmunit:testsuite:testsuiteobject:add-fixture test-suite-object test-fixture-object)
            (scmunit:testsuite:testsuitelist:update-test-suite (scmunit:testsuite:get-test-suite-list) test-suite-object)
            scmunit:ok))))))
