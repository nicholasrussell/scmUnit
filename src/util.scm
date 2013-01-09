;;;; --------------------------------------------------------------------------
;;;; scmUnit
;;;;   Nicholas Russell
;;;; --------------------------------------------------------------------------
;;;; util.scm
;;;;   Utilities/random
;;;; --------------------------------------------------------------------------

(load-option 'format) ;; For pretty output

;;;;
;; when
;;  Executes expressions when and only when pred is true
;; 
;; @param pred - Predicate
;; @param exp ... - Expressions
(define-syntax when
  (syntax-rules ()
    ((when pred exp ...)
      (if pred (begin exp ...)))))

;;;;
;; funcall
;;  Lisp-like funcall. Applies args to p.
;;
;; @param p - Procedure
;; @param args ... - Arguments (Expressions)
(define-syntax funcall
  (syntax-rules ()
    ((funcall p args ...)
      (apply (if (procedure? p) p (eval p (nearest-repl/environment))) (list args ...)))))

;;;;
;; scmunit:message-template
;;  Creates a message template
;;
;; @param msg - Optional msg to display - String
;; @param template - Message template - Format String
;; @param arg ... - Template arguments
(define-syntax scmunit:message-template
  (syntax-rules ()
    ((scmunit:message-template msg template arg ...)
      (format (current-output-port) 
              (string-append (string-append (if (null? msg) "" (string-append (scmunit:messagify msg) "~%")) template))
              arg ...))))

;;;;
;; scmunit:promisify
;;  Ensures that object is a promise
;;
;; @param object
;; @return delayed object
(define (scmunit:promisify object)
  (if (promise? object)
    object
    (delay object)))

;;;;
;; scmunit:forcify
;;  Ensures that a promise has been forced
;;
;; @param object
;; @return forced object
(define (scmunit:forcify object)
  (if (promise? object)
    (force object)
    object))

;;;;
;; scmunit:messagify
;;  Transforms an object into a string
;;
;; @param object
;; @return String representation of object
(define (scmunit:messagify object)
  (with-output-to-string (lambda () (display object))))

;;;;
;; scmunit:default-msg
;;  Ensures that a message is either String msg or '() (in the case of #!default)
;;
;; @param msg
;; @return msg or '()
(define (scmunit:default-msg msg)
  (if (not (eq? msg #!default))
    msg
    '()))
