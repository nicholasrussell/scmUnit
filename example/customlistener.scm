;;;;
;; scmUnit custom listener
;;  Be sure to load this file before scmUnit runs

;; customer run listener
(define (custom-run-listener)

  (define (before-suite suite-name)
    (format (current-output-port) "~A~%" suite-name))

  (define (after-suite suite-name)
    (format (current-output-port) "~A~%" suite-name))

  (define (before-test test-name)
    (format (current-output-port) "\t~A~%" test-name))

  (define (after-test test-name test-result)
    (format (current-output-port) "\t~A~%" test-name))

  (define (scmunit:runlistener:run-listener msg)
    (cond ((eq? msg scmunit:runlistener:before-suite) (lambda (suite-name) (before-suite suite-name)))
          ((eq? msg scmunit:runlistener:after-suite) (lambda (suite-name) (after-suite suite-name)))
          ((eq? msg scmunit:runlistener:before-test) (lambda (test-name) (before-test test-name)))
          ((eq? msg scmunit:runlistener:after-test) (lambda (test-name test-result) (after-test test-name test-result)))))

  scmunit:runlistener:run-listener)

; (scmunit:runlistener:set-run-listener custom-run-listener) ;; Uncomment this line to set the scmUnit run listener to your custom listener
