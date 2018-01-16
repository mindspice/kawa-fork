;; https://gitlab.com/kashell/Kawa/issues/33
;; Odd closure issue with cond and =>
(define procs
  (map (lambda (arg)
         (! m (memq arg '(a b c)))
         (if m
             (lambda () (format #t "tail is ~A~%" m)) ; cl$frame.lambda1
             (lambda () (format #t "~A not found.~%" arg)))) ; cl$frame.lambda2
       '(b c d a)))

((dynamic for-each) (lambda (f) (f)) procs)
;; Output: tail is (b c)
;; Output: tail is (c)
;; Output: d not found.
;; Output: tail is (a b c)
