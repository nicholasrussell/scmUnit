;;;; --------------------------------------------------------------------------
;;;; scmUnit
;;;;   Nicholas Russell
;;;; --------------------------------------------------------------------------
;;;; testfixtureobject.scm
;;;;   Test fixture object
;;;; --------------------------------------------------------------------------

;; test-fixture-object
(define (scmunit:testfixture:testfixtureobject:test-fixture-object)
  (let ((type '())
        (proc '()))

    (define (get-type) type)
    (define (set-type t)
      (when 
        (and
          (not (eq? t scmunit:testfixture:types:before-suite))
          (not (eq? t scmunit:testfixture:types:after-suite))
          (not (eq? t scmunit:testfixture:types:before-test))
          (not (eq? t scmunit:testfixture:types:after-test)))
        (error "Unrecognized test fixture type."))
      (set! type t) type)
    (define (get-proc) proc)
    (define (set-proc p) (when (not (procedure? p)) (error "Could not set test fixture procedure.")) (set! proc p) proc)

    (define (test-fixture-object msg)
      (cond ((eq? msg 'get-type) (get-type))
            ((eq? msg 'set-type) (lambda (type) (set-type type)))
            ((eq? msg 'get-proc) (get-proc))
            ((eq? msg 'set-proc) (lambda (proc) (set-proc proc)))
            ((eq? msg 'type) scmunit:testfixture:testfixtureobject:test-fixture-object-type)))

    test-fixture-object))

;;;;
;; scmunit:testfixture:testfixtureobject:get-type
;;  Returns the type of a test fixture
;;
;; @param test-fixture-object
;; @return type
(define (scmunit:testfixture:testfixtureobject:get-type test-fixture-object)
  (test-fixture-object 'get-type))

;;;;
;; scmunit:testfixture:testfixtureobject:get-proc
;;  Returns the proc of a test fixture
;;
;; @param test-fixture-object
;; @return proc
(define (scmunit:testfixture:testfixtureobject:get-proc test-fixture-object)
  (test-fixture-object 'get-proc))

;;;;
;; scmunit:testfixture:testfixtureobject:set-type
;;  Sets the type of a test fixture
;;
;; @param test-fixture-object
;; @param type
(define (scmunit:testfixture:testfixtureobject:set-type test-fixture-object type)
  ((test-fixture-object 'set-type) type))

;;;;
;; scmunit:testfixture:testfixtureobject:set-proc
;;  Sets the proc of a test fixture
;;
;; @param test-fixture-object
;; @param proc
(define (scmunit:testfixture:testfixtureobject:set-proc test-fixture-object proc)
  ((test-fixture-object 'set-proc) proc))

;;;;
;; scmunit:testfixture:testfixtureobject:create-test-fixture-object
;;  Creates a test fixture object
;;
;; @param type
;; @param body
;; @return test fixture object
(define (scmunit:testfixture:testfixtureobject:create-test-fixture-object type body)
  (when (not (symbol? type))
    (error "Test fixture type must be a valid symbol."))
  (let ((test-fixture-obj (scmunit:testfixture:testfixtureobject:test-fixture-object)))
    (scmunit:testfixture:testfixtureobject:set-type test-fixture-obj type)
    (scmunit:testfixture:testfixtureobject:set-proc test-fixture-obj body)

    test-fixture-obj))

;;;;
;; scmunit:testfixture:testfixtureobject:test-fixture-object?
;;  Returns true if object is a test-fixture-object
;;
;; @param object
;; @return boolean
(define (scmunit:testfixture:testfixtureobject:test-fixture-object? object)
  (scmunit:check-object-type object scmunit:testfixture:testfixtureobject:test-fixture-object-type))
