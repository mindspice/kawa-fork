;;; This doesn't actually test continuations, but it's a regression
;;; test for the anormalizer/code-fragmenter.
;; Kawa-options: --full-continuations %F
(define (f x)
  (format #t "called f.~%")
  (cons 'f x))
(define (g x)
  (format #t "called g.~%")
  (cons 'g x))
(define (h x)
  (format #t "called h.~%")
  (cons 'h x))
(define yglobal 100)

(define (ff x)
  (! y1 (f x) )
  (set! yglobal (g x))
  (! y3 (h x))
  (list y1 yglobal y3))

(format #t "(ff 3) => ~w~%" (ff 3))
;; Output: called f.
;; Output: called g.
;; Output: called h.
;; Output: (ff 3) => ((f . 3) (g . 3) (h . 3))
