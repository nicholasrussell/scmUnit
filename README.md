## scmUnit
A unit testing framework for MIT-Scheme

## Example

```lisp
(load "path/to/scmUnit/scmUnit.scm")
(define x 2)
(define-test "x = 2" (assert-equals 2 x "x was not 2"))
(define-test "null test" (assert-null '()))
(define-test "larger test" (let ((x 4)) (assert-not-null x) (assert-true (> x 2))))
(run-test "x = 2")
(run-test "null test")
(run-test "larger test")
(print-results)
```

Thanks to Alexey Radul for [Test Manager](http://web.mit.edu/~axch/www/test_manager.html) to help get me started.