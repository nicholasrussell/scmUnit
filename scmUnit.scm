;;;; --------------------------------------------------------------------------
;;;; scmUnit
;;;;   Nicholas Russell
;;;; --------------------------------------------------------------------------
;;;; scmUnit.scm
;;;;   Loads necessary source files and runs tests
;;;; --------------------------------------------------------------------------

(define scmunit:run-tests-on-load #t)

(define (src-load src)
  (set! load/suppress-loading-message? #t)
  (with-working-directory-pathname (directory-namestring (current-load-pathname)) (lambda () (load src)))
  (set! load/suppress-loading-message? #f))

(define (option-load option)
  (set! load/suppress-loading-message? #t)
  (load-option option)
  (set! load/suppress-loading-message? #f))

(option-load 'format)


;; Suppress displaying procedures for loading
(define display-old display)
(define newline-old newline)
(define format-old format)
(define (display object #!optional port environment) '())
(define (newline #!optional port) '())
(define (format destination format-string . arguments) '())

;; src
(src-load "src/constants.scm")
(src-load "src/globals.scm")
(src-load "src/util.scm")
(src-load "src/conditions.scm")
(src-load "src/assert.scm")
(src-load "src/testfixtureobject.scm")
(src-load "src/testsuiteobject.scm")
(src-load "src/testsuitelist.scm")
(src-load "src/testsuite.scm")
(src-load "src/testfixture.scm")
(src-load "src/testobject.scm")
(src-load "src/test.scm")
(src-load "src/testresultobject.scm")
(src-load "src/runlistener.scm")
(src-load "src/runner.scm")
;(src-load "src/gwt.scm")

;; test
(when scmunit:run-tests-on-load
  (src-load "test/utiltest.scm")
  (src-load "test/conditionstest.scm")
  (src-load "test/asserttest.scm")
  (src-load "test/testsuiteobjecttest.scm")
  (src-load "test/testobjecttest.scm")
  (src-load "test/testtest.scm")
  (src-load "test/runnertest.scm")

  ;(src-load "test/bigtest.scm")
)

;; Reset displaying procedures
(set! display display-old)
(set! newline newline-old)
(set! format format-old)

'scmUnit
