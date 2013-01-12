(define-test "Null Test" (assert-null '()) (assert-not-null #t))

(define-test-suite "AddSuite")
(begin-test-suite "AddSuite")
(define-test "Add2"
  (let ((x 2))
    (assert-equals 2 x)
    (assert-equals 4 (+ x 2) "X + 2 was not 4!")))
(define-test "Add0"
  (let ((x 5))
    (assert-equals 5 x)
    (assert-equals 5 (+ x 0))))
(end-test-suite)

(define-test "True Test" (assert-true #t) (assert-false #f))

(define-test-suite "MultiplySuite")
(begin-test-suite "MultiplySuite")
(define-test "Square"
  (let ((y 3))
    (assert-equals 3 y)
    (assert-equals 9 (* y y))))
(end-test-suite)
