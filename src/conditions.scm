;;;; --------------------------------------------------------------------------
;;;; scmUnit
;;;;   Nicholas Russell
;;;; --------------------------------------------------------------------------
;;;; conditions.scm
;;;;   Test failure conditions
;;;; --------------------------------------------------------------------------

;; Custom test-failure condition type
(define condition-type:test-failure
  (make-condition-type 'test-failure condition-type:error '(message) 
    (lambda (condition port) (format port "~A" (access-condition condition 'message)))))

;;;;
;; condition/test-failure?
;;  Predicate to test if a condition is condition-type:test-failure
;;
;; @return condition predicate
(define condition/test-failure?
  (condition-predicate condition-type:test-failure))

;;;;
;; test-failure
;;  Signal test-failure condition
;;
;; @return condition signaller
(define test-failure
  (condition-signaller condition-type:test-failure '(message) standard-error-handler))

;;;;
;; call-capture-errors
;;  Calls a procedure `proc' and captures errors
;;
;; @param proc
;; @return return of proc
(define (call-capture-errors proc)
  (when standard-error-hook
    (warn "Cannot definitively capture errors if standard-error-hook is bound."))
  (call-with-current-continuation
    (lambda (x)
      (fluid-let ((standard-error-hook x))
        (proc)))))