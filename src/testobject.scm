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

    (define (dispatch msg)
      (cond ((eq? msg 'get-name) (get-name))
            ((eq? msg 'get-proc) (get-proc))
            ((eq? msg 'set-name) (lambda (n) (set-name n)))
            ((eq? msg 'set-proc) (lambda (p) (set-proc p)))))

    dispatch))

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