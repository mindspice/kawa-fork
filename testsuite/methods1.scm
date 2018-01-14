(display (java.lang.Integer:toHexString 266)) (newline)
;; Output: 10a
(define (to-hex (x :: <int>)) (java.lang.Integer:toHexString x))
(display (to-hex 267)) (newline)
;; Output: 10b
(define-namespace Long "class:java.lang.Long")
(display (Long:toHexString 269)) (newline)
;; Output: 10d
(define (long-to-hex (x :: <long>)) (Long:toHexString x))
(display (long-to-hex 270)) (newline)
;; Output: 10e
(display (Long:toString (Long:new '00123))) (newline)
;; Output: 123
(define (to-int-string x :: <long>) (java.lang.Object:toString (Long:new x)))
;; Diagnostic: methods1.scm:14:37: warning - no static method 'toString' in java.lang.Object
(display (to-int-string '00124)) (newline)
;; Output: 124

(define-namespace date "class:java.util.Date")
(! year (+ 1900 (date:get-year (date:new))))
;; Diagnostic: methods1.scm:20:17: warning - no static method 'get-year' in java.util.Date
(write (> year 2015)) (newline)
;; Output: #t
