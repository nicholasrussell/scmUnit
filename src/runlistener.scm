;;;; --------------------------------------------------------------------------
;;;; scmUnit
;;;;   Nicholas Russell
;;;; --------------------------------------------------------------------------
;;;; runlistener.scm
;;;;   Creates a default run listener
;;;; --------------------------------------------------------------------------

;;;;
;; scmunit:runlistener:get-run-listener
(define (scmunit:runlistener:get-run-listener)
  scmunit:runlistener:*run-listener*)

;;;;
;; scmunit:runlistener:set-run-listener
;;
;; @param listener
(define (scmunit:runlistener:set-run-listener listener)
  (set! scmunit:runlistener:*run-listener* listener)
  (scmunit:runlistener:get-run-listener))

;;;;
;; scmunit:runlistener:notify-listener
;;  Notifies the run listener of the event 
;;
;; @param event
(define-syntax scmunit:runlistener:notify-listener
  (syntax-rules ()
    ((scmunit:runlistener:notify-listener event args ...)
      (let ((listener (scmunit:runlistener:get-run-listener)))
        (when (not (null? listener))
          (call-capture-errors (((listener) event) args ...)))))))

;; default run listener
(define (scmunit:runlistener:default-run-listener)

  (define bar "----------------------------------------~%")

  (define (before-suite suite-name)
    (format (current-output-port) (string-append bar (string-append "  Begin suite: ~A~%" bar)) suite-name))

  (define (after-suite suite-name)
    (format (current-output-port) (string-append bar (string-append "  End suite: ~A~%" bar)) suite-name))

  (define (before-test test-name)
    (format (current-output-port) (string-append "\t" (string-append bar (string-append "\t  Begin test: ~A~%" (string-append "\t" bar)))) test-name))

  (define (after-test test-name test-result)
    (format (current-output-port) (string-append "\t" (string-append bar (string-append "\t  End test: ~A~%" (string-append "\t" bar)))) test-name))

  (define (scmunit:runlistener:run-listener msg)
    (cond ((eq? msg scmunit:runlistener:before-suite) (lambda (suite-name) (before-suite suite-name)))
          ((eq? msg scmunit:runlistener:after-suite) (lambda (suite-name) (after-suite suite-name)))
          ((eq? msg scmunit:runlistener:before-test) (lambda (test-name) (before-test test-name)))
          ((eq? msg scmunit:runlistener:after-test) (lambda (test-name test-result) (after-test test-name test-result)))))

  scmunit:runlistener:run-listener)

(scmunit:runlistener:set-run-listener scmunit:runlistener:default-run-listener)
