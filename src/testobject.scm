;;;; --------------------------------------------------------------------------
;;;; scmUnit
;;;;   Nicholas Russell
;;;; --------------------------------------------------------------------------
;;;; testobject.scm
;;;;   Test object representation
;;;; --------------------------------------------------------------------------

;; test-object
(define (scmunit:test:testobject:test-object)
  (let ((name '())
        (proc '()))

    (define (get-name) name)
    (define (get-proc) proc)
    (define (set-name n) (set! name n) (get-name))
    (define (set-proc p) (set! proc p) (get-proc))

    (define (test-object msg)
      (cond ((eq? msg 'get-name) (get-name))
            ((eq? msg 'get-proc) (get-proc))
            ((eq? msg 'set-name) (lambda (n) (set-name n)))
            ((eq? msg 'set-proc) (lambda (p) (set-proc p)))
            ((eq? msg 'type) scmunit:test:testobject:test-object-type)))

    test-object))

;;;;
;; scmunit:test:testobject:get-name
;;  Gets the name of a test-object
;;
;; @param test-object
;; @return name
(define (scmunit:test:testobject:get-name test-object)
  (test-object 'get-name))

;;;;
;; scmunit:test:testobject:get-proc
;;  Gets the procedure of a test-object
;;
;; @param test-object
;; @return proc
(define (scmunit:test:testobject:get-proc test-object)
  (test-object 'get-proc))

;;;;
;; scmunit:test:testobject:set-name
;;  Sets the name of a test-object
;;
;; @param test-object
;; @param name
(define (scmunit:test:testobject:set-name test-object name)
  ((test-object 'set-name) name))

;;;;
;; scmunit:test:testobject:set-proc
;;  Sets the procedure of a test-object
;;
;; @param test-object
;; @param proc
(define (scmunit:test:testobject:set-proc test-object proc)
  ((test-object 'set-proc) proc))

;;;;
;; scmunit:test:test-object:create-test-object
;;  Creates a test object with name `name' and proc `body'
;;
;; @param name
;; @param body
;; @return test-object
(define (scmunit:test:testobject:create-test-object name body)
  (let ((test-object (scmunit:test:testobject:test-object)))
    (scmunit:test:testobject:set-name test-object name)
    (scmunit:test:testobject:set-proc test-object body)

    test-object))

;;;;
;; scmunit:test:testobject:test-object?
;;  Returns true if object is a test-object
;;
;; @param object
;; @return boolean
(define (scmunit:test:testobject:test-object? object)
  (if (or (null? object) (not (procedure? object)))
    #f
    (let ((arity (procedure-arity object)))
      (if (or (not (= (car arity) 1)) (not (= (cdr arity) 1)))
        #f
        (let ((result (call-capture-errors (lambda () (object 'type)))))
          (if (condition? result)
            #f
            (eq? result scmunit:test:testobject:test-object-type)))))))
