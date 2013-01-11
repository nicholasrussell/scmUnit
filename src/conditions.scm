;;;; --------------------------------------------------------------------------
;;;; scmUnit
;;;;   Nicholas Russell
;;;; --------------------------------------------------------------------------
;;;; conditions.scm
;;;;   Conditions
;;;; --------------------------------------------------------------------------

;; Custom assertion-failure condition type
(define condition-type:assertion-failure
  (make-condition-type 'assertion-failure condition-type:error '(message) 
    (lambda (condition port) (format port "~A" (access-condition condition 'message)))))

;;;;
;; condition/assertion-failure?
;;  Predicate to test if a condition is condition-type:assertion-failure
;;
;; @return condition predicate
(define condition/assertion-failure?
  (condition-predicate condition-type:assertion-failure))

;;;;
;; assertion-failure
;;  Signal assertion-failure condition
;;
;; @return condition signaller
(define assertion-failure
  (condition-signaller condition-type:assertion-failure '(message) standard-error-handler))

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
