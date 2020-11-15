(import (scheme base)
        (scheme write)
        (srfi 1))

(write (apply circular-list '(3 1 4 1 5 9)))
(newline)
;; Output: #0=(3 1 4 1 5 9 . #0#)
(write (circular-list 3 1 4 1 5))
(newline)
;; Output: #0=(3 1 4 1 5 . #0#)

;; GitLab issue #78
(let ((x (list 1 (list "2") 3)))
  (set-cdr! (cadr x) (cdr x))
  (set-cdr! (cddr x) x)
  (write x) (newline)
  (display x) (newline))
;; Output: #0=(1 . #1=(("2" . #1#) 3 . #0#))
;; Output: #0=(1 . #1=((2 . #1#) 3 . #0#))
