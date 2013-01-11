;;;; --------------------------------------------------------------------------
;;;; scmUnit
;;;;   Nicholas Russell
;;;; --------------------------------------------------------------------------
;;;; utiltest.scm
;;;;   util tests 
;;;; --------------------------------------------------------------------------

(define (utiltest:assert exp msg)
  (if (not exp)
    (error msg)))

;; when tests
(utiltest:assert (= (when #t 2) 2) "when #t one arg failed")
(utiltest:assert (= (when #t 2 3) 3) "when #t multiple args failed")
(utiltest:assert (equal? (when #f 2 3) #!unspecific) "when #f failed")

;; funcall tests
(utiltest:assert (= (funcall + 0) 0) "Funcall + 0 failed")
(utiltest:assert (= (funcall + 1 2 3) 6) "Funcall + 1 2 3 failed")

;; scmunit:message-template tests

;; scmunit:promisify tests
(utiltest:assert (promise? (scmunit:promisify (+ 1 1))) "scmunit:promisify object failed") ; fixme applicative order evalutaion ...
(utiltest:assert (promise? (scmunit:promisify (delay (+ 1 1)))) "scmunit:promisify delayed object failed")

;; scmunit:forcify tests
(let ((promise (delay (+ 1 1))))
  (utiltest:assert (and (not (promise? (scmunit:forcify promise))) (= (scmunit:forcify promise) 2)) "scmunit:forcify delayed object failed"))
(let ((not-promise (+ 1 1)))
  (utiltest:assert (and (not (promise? (scmunit:forcify not-promise))) (= (scmunit:forcify not-promise) 2)) "scmunit:forcify not delayed object failed"))

;; scmunit:messagify tests
(utiltest:assert (and (string? (scmunit:messagify 2)) (string=? (scmunit:messagify 2) "2")) "scmunit:messagify test failed")

;; scmunit:default-msg tests
(utiltest:assert (equal? (scmunit:default-msg "some message") "some message") "scmunit:default-msg string message failed")
(utiltest:assert (equal? (scmunit:default-msg #!default) '()) "scmunit:default-msg #!default message failed")
