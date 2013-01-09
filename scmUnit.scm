;;;; --------------------------------------------------------------------------
;;;; scmUnit
;;;;   Nicholas Russell
;;;; --------------------------------------------------------------------------
;;;; scmUnit.scm
;;;;   Loads necessary source files  
;;;; --------------------------------------------------------------------------

(define (src-load src)
  (with-working-directory-pathname (directory-namestring (current-load-pathname)) (lambda () (load src))))

(src-load "src/util.scm")
(src-load "src/conditions.scm")
(src-load "src/assert.scm")
(src-load "src/testobject.scm")
(src-load "src/test.scm")
(src-load "src/runner.scm")

;;;; TODO ACTUALLY IMPLEMENT THIS
;;;; SEMI-STUBBING ...
(define tests '())
(define test-results '())
(define (add-test-object test-object)
  (set-tests (append (get-tests) (list test-object))))
(define (get-tests)
  tests)
(define (set-tests t)
  (set! tests t))
(define (get-test-by-name test-name)
  (let ((test-list (filter (lambda (test) (equal? (scmunit:testobject:get-name test) test-name)) (get-tests))))
    (if (or (null? test-list) (> (length test-list) 1))
      '()
      (scmunit:testobject:get-proc (car test-list)))))
(define (print-results)
  (for-each (lambda (result) (display "Test: ") (display (car result)) (display " Result: ") (display (cdr result)) (newline)) test-results)
  (let ((passed (filter (lambda (result) (eq? (cdr result) 'pass)) test-results)))
    (display "Passed ") (display (length passed)) (display "/") (display (length test-results)) (newline)
    (length passed)))

(src-load "test/definetest.scm")

'scmUnit