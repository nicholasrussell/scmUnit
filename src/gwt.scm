;;;; --------------------------------------------------------------------------
;;;; scmUnit
;;;;   Nicholas Russell
;;;; --------------------------------------------------------------------------
;;;; gwt.scm
;;;;   Given When Then support
;;;; --------------------------------------------------------------------------

(define-syntax gwt:given
  (syntax-rules ()
    ((gwt:given str exp ...)
      (list 'given str (lambda () exp ...)))))

(define-syntax gwt:when
  (syntax-rules ()
    ((gwt:when str exp ...)
      (list 'when str (lambda () exp ...)))))

(define-syntax gwt:then
  (syntax-rules ()
    ((gwt:then str exp ...)
      (list 'then str (lambda () exp ...)))))

;; todo store vars local to test to be used between g, w, t
(define-syntax define-gwt-test
  (syntax-rules ()
    ((define-gwt-test name g w t)
      (begin
        (when (or (not (list? g)) (not (list? w)) (not (list? t)))
          (error "Use gwt:given, gwt:when, and gwt:then for defining gwt tests."))
        (when (or (not (eq? (car g) 'given)) (not (eq? (car w) 'when)) (not (eq? (car t) 'then)))
          (error "Use gwt:given, gwt:when, and gwt:then (in that order) for defining gwt tests."))
        (list name g w t)))))

(define (run-gwt gwt)
  (let* ((test-name (car gwt))
         (g (cadr gwt))
         (w (caddr gwt))
         (t (cadddr gwt))
         (given-str (cadr g))
         (when-str (cadr w))
         (then-str (cadr t))
         (given-proc (caddr g))
         (when-proc (caddr w))
         (then-proc (caddr t)))
    (call-with-current-continuation
      (lambda (return)
        (format (current-output-port) "--------------------~%Test  : ~A~%~%Given : ~A~%When  : ~A~%Then  : ~A~%--------------------~%~%" test-name given-str when-str then-str)
        (format (current-output-port) "Given: ~A~%~%" given-str)
        (define given-result (call-capture-errors (given-proc)))
        (define given-eval-result (scmunit:evaluate-test-result given-result))
        (when (not (passed? given-eval-result))
          (return))
        (format (current-output-port) "~%~%When: ~A~%~%" when-str)
        (define when-result (call-capture-errors (when-proc)))
        (define when-eval-result (scmunit:evaluate-test-result when-result))
        (when (not (scmunit:test-passed? when-eval-result))
          (return))
        (format (current-output-port) "~%~%Then: ~A~%~%" then-str)
        (define then-result (call-capture-errors (then-proc)))
        (define then-eval-result (scmunit:evaluate-test-result then-result))
        (when (not (passed? then-eval-result))
          (return))))))
