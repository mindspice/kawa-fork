; Test for Issue #110 on GitLab, from Robby Zambito (@zambyte)
; "Inconsistent stack length when raising an exception from a parameterized procedure"
 (let ((foo (make-parameter (lambda () 42))))
  (guard (e ((eq? e 5) (display "guard ") (display e) #t)
	    (else (display "guard #F") #f))
    (parameterize ((foo (lambda () (raise 5))))
      ((foo)))))
(newline)
;; Output: guard 5
