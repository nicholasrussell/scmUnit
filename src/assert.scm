;;;; --------------------------------------------------------------------------
;;;; scmUnit
;;;;   Nicholas Russell
;;;; --------------------------------------------------------------------------
;;;; assert.scm
;;;;   Assertions
;;;; --------------------------------------------------------------------------

;(define scmunit:show-assertion-stack-traces #f)

;;;;
;; assert-exp
;;  When exp is false, throw test-fail with message.
;;
;; @param exp - Expression to be evaluated
;; @param expected - Expected value
;; @param actual - Actual value (exp)
;; @param msg - Optional msg
(define (scmunit:assert-exp exp expected actual msg)
  (when (not (exp))
    ;; FIXME to display stack-traces, wrap call-capture-errors lambda around assertion-failure.
    ;; Commented for now as it causes call-capture-errors nesting, which is bad.
    (let ((failure-condition (assertion-failure 
                              (scmunit:message-template msg 
                                                        "Assertion failed. Expected <~A>, was <~A>."
                                                        (scmunit:messagify expected)
                                                        (scmunit:messagify actual)))))
      ;(when scmunit:show-assertion-stack-traces
      ;    (stack-trace failure-condition (current-output-port)))
      failure-condition)))

;;;;
;; assert-null
;;
;; @param exp
;; @param #!optional msg
(define (assert-null exp #!optional msg)
  (scmunit:assert-exp (lambda () (null? exp)) '() exp (scmunit:default-msg msg)))

;;;;
;; assert-not-null
;;
;; @param exp
;; @param #!optional msg
(define (assert-not-null exp #!optional msg)
  (scmunit:assert-exp (lambda () (not (null? exp))) "not ()" exp (scmunit:default-msg msg)))

;;;;
;; assert-true
;;
;; @param exp
;; @param #!optional msg
(define (assert-true exp #!optional msg)
  (scmunit:assert-exp (lambda () exp) #t exp (scmunit:default-msg msg)))

;;;;
;; assert-false
;;
;; @param exp
;; @param #!optional msg
(define (assert-false exp #!optional msg)
  (scmunit:assert-exp (lambda () (not exp)) #f exp (scmunit:default-msg msg)))

;;;;
;; assert-equal
;;
;; @param exp
;; @param #!optional msg
(define (assert-equal exp act #!optional msg)
  (scmunit:assert-exp (lambda () (equal? exp act)) exp act (scmunit:default-msg msg)))
(define assert-equals assert-equal)

;;;;
;; assert-eq
;;
;; @param exp
;; @param #!optional msg
(define (assert-eq exp act #!optional msg)
  (scmunit:assert-exp (lambda () (eq? exp act)) exp act (scmunit:default-msg msg)))

;;;;
;; assert-eqv
;;
;; @param exp
;; @param #!optional msg
(define (assert-eqv exp act #!optional msg)
  (scmunit:assert-exp (lambda () (eqv? exp act)) exp act (scmunit:default-msg msg)))

;;;;
;; assert-=
;;
;; @param exp
;; @param #!optional msg
(define (assert-= exp act #!optional msg)
  (scmunit:assert-exp (lambda () (= exp act)) exp act (scmunit:default-msg msg)))
(define assert= assert-=)

;;;;
;; assert-fail
;;
;; @param #!optional msg
(define (assert-fail #!optional msg)
  (scmunit:assert-exp (lambda () #f) "fail" "fail" (scmunit:default-msg msg)))