;;;; --------------------------------------------------------------------------
;;;; scmUnit
;;;;   Nicholas Russell
;;;; --------------------------------------------------------------------------
;;;; assert.scm
;;;;   Assertions
;;;; --------------------------------------------------------------------------

;(define scmunit:show-assertion-stack-traces #t)

;;;;
;; assert-exp
;;  When exp is false, throw assertion-failure with message.
;;
;; @param exp - Expression to be evaluated
;; @param expected - Expected value
;; @param actual - Actual value (exp)
;; @param msg - Optional msg
(define-syntax scmunit:assert-exp
  (syntax-rules ()
    ((scmunit:assert-exp exp expected msg)
      (let ((sexp (lambda () exp))
            (sexpected (lambda () expected)))
        (when (not (sexp))
          (let ((failure-condition (assertion-failure
                                    (scmunit:message-template (scmunit:default-msg msg)
                                                              scmunit:assertion-message
                                                              (scmunit:messagify (sexpected))
                                                              (scmunit:messagify (sexp))))))
            ;(when scmunit:show-assertion-stack-traces
            ;  (stack-trace failure-condition (current-output-port)))
            failure-condition))))))

;;;;
;; assert-null
;;
;; @param exp
;; @param #!optional msg
(define (assert-null exp #!optional msg)
  (scmunit:assert-exp (null? exp) '() msg))

;;;;
;; assert-not-null
;;
;; @param exp
;; @param #!optional msg
(define (assert-not-null exp #!optional msg)
  (scmunit:assert-exp (not (null? exp)) "not ()" msg))

;;;;
;; assert-true
;;
;; @param exp
;; @param #!optional msg
(define (assert-true exp #!optional msg)
  (scmunit:assert-exp exp #t msg))

;;;;
;; assert-false
;;
;; @param exp
;; @param #!optional msg
(define (assert-false exp #!optional msg)
  (scmunit:assert-exp (not exp) #f msg))

;;;;
;; assert-equal
;;
;; @param exp
;; @param #!optional msg
(define (assert-equal exp act #!optional msg)
  (scmunit:assert-exp (equal? exp act) exp msg))
(define assert-equals assert-equal)

;;;;
;; assert-eq
;;
;; @param exp
;; @param #!optional msg
(define (assert-eq exp act #!optional msg)
  (scmunit:assert-exp (eq? exp act) exp msg))

;;;;
;; assert-eqv
;;
;; @param exp
;; @param #!optional msg
(define (assert-eqv exp act #!optional msg)
  (scmunit:assert-exp (eqv? exp act) exp msg))

;;;;
;; assert-=
;;
;; @param exp
;; @param #!optional msg
(define (assert-= exp act #!optional msg)
  (scmunit:assert-exp (= exp act) exp msg))
(define assert= assert-=)

;;;;
;; assert-fail
;;
;; @param #!optional msg
(define (assert-fail #!optional msg)
  (scmunit:assert-exp #f #f msg))
