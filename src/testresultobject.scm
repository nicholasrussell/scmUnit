;;;; --------------------------------------------------------------------------
;;;; scmUnit
;;;;   Nicholas Russell
;;;; --------------------------------------------------------------------------
;;;; testresultobject.scm
;;;;   Test result object
;;;; --------------------------------------------------------------------------

;; test-result-object
(define (scmunit:test:testresultobject:test-result-object)
  (let ((test-suite-name '())
        (test-name '())
        (test-result '()))

    (define (get-test-suite-name) test-suite-name)
    (define (get-test-name) test-name)
    (define (get-test-result) test-result)
    (define (set-test-suite-name suite-name) (set! test-suite-name suite-name) (get-test-suite-name))
    (define (set-test-name name) (set! test-name name) (get-test-name))
    (define (set-test-result result) (set! test-result result) (get-test-result))

    (define (test-result-object msg)
      (cond ((eq? msg 'get-test-suite-name) (get-test-suite-name))
            ((eq? msg 'get-test-name) (get-test-name))
            ((eq? msg 'get-test-result) (get-test-result))
            ((eq? msg 'set-test-suite-name) (lambda (test-suite-name) (set-test-suite-name test-suite-name)))
            ((eq? msg 'set-test-name) (lambda (test-name) (set-test-name test-name)))
            ((eq? msg 'set-test-result) (lambda (test-result) (set-test-result test-result)))
            ((eq? msg 'type) scmunit:test:testresultobject:test-result-object-type)))

    test-result-object))

;;;;
;; scmunit:test:testresultobject:get-test-suite-name
;;
;; @param test-result-object
(define (scmunit:test:testresultobject:get-test-suite-name test-result-object)
  (test-result-object 'get-test-suite-name))

;;;;
;; scmunit:test:testresultobject:get-test-name
;;
;; @param test-result-object
(define (scmunit:test:testresultobject:get-test-name test-result-object)
  (test-result-object 'get-test-name))

;;;;
;; scmunit:test:testresultobject:get-test-result
;;
;; @param test-result-object
(define (scmunit:test:testresultobject:get-test-result test-result-object)
  (test-result-object 'get-test-result))

;;;;
;; scmunit:test:testresultobject:set-test-suite-name
;;
;; @param test-result-object
;; @param test-suite-name
(define (scmunit:test:testresultobject:set-test-suite-name test-result-object test-suite-name)
  ((test-result-object 'set-test-suite-name) test-suite-name))

;;;;
;; scmunit:test:testresultobject:set-test-name
;;
;; @param test-result-object
;; @param test-name
(define (scmunit:test:testresultobject:set-test-name test-result-object test-name)
  ((test-result-object 'set-test-name) test-name))

;;;;
;; scmunit:test:testresultobject:set-test-result
;;
;; @param test-result-object
;; @param test-result
(define (scmunit:test:testresultobject:set-test-result test-result-object test-result)
  ((test-result-object 'set-test-result) test-result))

;;;;
;; scmunit:test:testresultobject:create-test-result-object
;;
;; @param test-suite-name
;; @param test-name
;; @param result
;; @return test-result-object
(define (scmunit:test:testresultobject:create-test-result-object test-suite-name test-name result)
  (let ((test-result-object (scmunit:test:testresultobject:test-result-object)))
    (scmunit:test:testresultobject:set-test-suite-name test-result-object test-suite-name)
    (scmunit:test:testresultobject:set-test-name test-result-object test-name)
    (scmunit:test:testresultobject:set-test-result test-result-object result)

    test-result-object))

;;;;
;; scmunit:testresult:testresultobject:test-result-object?
;;  Returns true if object is a test-object
;;
;; @param object
;; @return boolean
(define (scmunit:test:testresultobject:test-result-object? object)
  (scmunit:check-object-type object scmunit:test:testresultobject:test-result-object-type))
