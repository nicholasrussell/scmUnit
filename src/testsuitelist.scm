;;;; --------------------------------------------------------------------------
;;;; scmUnit
;;;;   Nicholas Russell
;;;; --------------------------------------------------------------------------
;;;; testsuitelist.scm
;;;;   Test suite list object
;;;; --------------------------------------------------------------------------

;; test-suite-object
(define (scmunit:testsuite:testsuitelist:test-suite-list)
  (let ((test-suite-objects '()))

    (define (get-test-suites) test-suite-objects)
    (define (set-test-suites test-suites) (set! test-suite-objects test-suites) (get-test-suites))
    (define (get-test-suite test-suite-name)
      (let loop ((test-suites (get-test-suites)))
        (if (null? test-suites)
          '()
          (if (equal? (scmunit:testsuite:testsuiteobject:get-name (car test-suites)) test-suite-name)
            (car test-suites)
            (loop (cdr test-suites))))))
    (define (add-test-suite test-suite-object)
      (when (not (scmunit:testsuite:testsuiteobject:test-suite-object? test-suite-object))
        (error "Cannot add non test suite object to test suite objects list"))
      (set-test-suites (append (get-test-suites) (list test-suite-object))))
    (define (update-test-suite test-suite-object)
      (when (not (scmunit:testsuite:testsuiteobject:test-suite-object? test-suite-object))
        (error "Cannot apply update of non test suite object to test suite object list"))
      (let loop ((test-suites (get-test-suites))
                 (new-suites '()))
        (if (null? test-suites)
          (set-test-suites new-suites)
          (if (equal? (scmunit:testsuite:testsuiteobject:get-name test-suite-object) (scmunit:testsuite:testsuiteobject:get-name (car test-suites)))
            (loop (cdr test-suites) (append new-suites (list test-suite-object)))
            (loop (cdr test-suites) (append new-suites (list (car test-suites))))))))

        (define (test-suite-list msg)
          (cond ((eq? msg 'get-test-suites) (get-test-suites))
                ((eq? msg 'get-test-suite) (lambda (test-suite-name) (get-test-suite test-suite-name)))
                ((eq? msg 'add-test-suite) (lambda (test-suite-object) (add-test-suite test-suite-object)))
                ((eq? msg 'update-test-suite) (lambda (test-suite-object) (update-test-suite test-suite-object)))
                ((eq? msg 'type) scmunit:testsuite:testsuitelist:test-suite-list-type)))

    test-suite-list))

;;;;
;; scmunit:testsuite:testsuitelist:get-test-suites
;;
;; @param test-suite-list
;; @retyrb test suite objects
(define (scmunit:testsuite:testsuitelist:get-test-suites test-suite-list)
  (test-suite-list 'get-test-suites))

;;;;
;; scmunit:testsuite:testsuitelist:get-test-suite
;;
;; @param test-suite-list
;; @param test-suite-name
;; @return test suite object
(define (scmunit:testsuite:testsuitelist:get-test-suite test-suite-list test-suite-name)
  ((test-suite-list 'get-test-suite) test-suite-name))

;;;;
;; scmunit:testsuite:testsuitelist:add-test-suite
;;
;; @param test-suite-list
;; @param test-suite-object
(define (scmunit:testsuite:testsuitelist:add-test-suite test-suite-list test-suite-object)
  ((test-suite-list 'add-test-suite) test-suite-object))

;;;;
;; scmunit:testsuite:testsuitelist:update-test-suite
;;
;; @param test-suite-list
;; @param test-suite-object
(define (scmunit:testsuite:testsuitelist:update-test-suite test-suite-list test-suite-object)
  ((test-suite-list 'update-test-suite) test-suite-object))

;;;;
;; scmunit:testsuite:testsuitelist:create-test-suite-list
(define (scmunit:testsuite:testsuitelist:create-test-suite-list)
  (let ((test-suite-list (scmunit:testsuite:testsuitelist:test-suite-list)))

    test-suite-list))

;;;;
;; scmunit:testsuite:testsuitelist:test-suite-list?
;;  Returns true if object is a test-suite-list
;;
;; @param object
;; @return boolean
(define (scmunit:testsuite:testsuitelist:test-suite-list? object)
  (scmunit:check-object-type object scmunit:testsuite:testsuitelist:test-suite-list-type))
