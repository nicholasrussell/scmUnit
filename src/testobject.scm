;;;; --------------------------------------------------------------------------
;;;; scmUnit
;;;;   Nicholas Russell
;;;; --------------------------------------------------------------------------
;;;; testobject.scm
;;;;   Test object representation
;;;; --------------------------------------------------------------------------

;; test-object
(define (scmunit:testobject:test-object)
  (let ((name '())
        (proc '()))

    (define (get-name) name)
    (define (get-proc) proc)
    (define (set-name n) (begin (set! name n) name))
    (define (set-proc p) (begin (set! proc p) proc))

    (define (test-object msg)
      (cond ((eq? msg 'get-name) (get-name))
            ((eq? msg 'get-proc) (get-proc))
            ((eq? msg 'set-name) (lambda (n) (set-name n)))
            ((eq? msg 'set-proc) (lambda (p) (set-proc p)))
            ((eq? msg 'type) scmunit:testobject:test-object-type)))

    test-object))

;;;;
;; scmunit:testobject:get-name
;;  Gets the name of a test-object
;;
;; @param test-object
;; @return name
(define (scmunit:testobject:get-name test-object)
  (test-object 'get-name))

;;;;
;; scmunit:testobject:get-proc
;;  Gets the procedure of a test-object
;;
;; @param test-object
;; @return proc
(define (scmunit:testobject:get-proc test-object)
  (test-object 'get-proc))

;;;;
;; scmunit:testobject:set-name
;;  Sets the name of a test-object
;;
;; @param test-object
;; @return lambda - argument is name to set
(define (scmunit:testobject:set-name test-object)
  (lambda (n) ((test-object 'set-name) n)))

;;;;
;; scmunit:testobject:set-proc
;;  Sets the procedure of a test-object
;;
;; @param test-object
;; @return lambda - argument is procedure to set
(define (scmunit:testobject:set-proc test-object)
  (lambda (n) ((test-object 'set-proc) n)))

;;;;
;; scmunit:test-object:create-test-object
;;  Creates a test object with name `name' and proc `body'
;;
;; @param name
;; @param body
;; @return test-object
(define (scmunit:testobject:create-test-object name body)
  (let ((test-object (scmunit:testobject:test-object)))
    ((scmunit:testobject:set-name test-object) name)
    ((scmunit:testobject:set-proc test-object) body)

    test-object))

;;;;
;; scmunit:testobject:test-object?
;;  Returns true if object is a test-object
;;
;; @param object
;; @return boolean
(define (scmunit:testobject:test-object? object)
  (if (or (null? object) (not (procedure? object)))
    #f
    (let ((arity (procedure-arity object)))
      (if (or (not (= (car arity) 1)) (not (= (cdr arity) 1)))
        #f
        (let ((result (call-capture-errors (lambda () (object 'type)))))
          (if (condition? result)
            #f
            (eq? result scmunit:testobject:test-object-type)))))))
