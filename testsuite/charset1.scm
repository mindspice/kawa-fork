;; Some of these are based on GitLab issue #43
;; "SRFI 14 char-set:digit etc. symbols not recognized during import"
(let ()
  (import (scheme base)
          (scheme write)
          (only (srfi 14)
                char-set-contains? char-set:digit))
  (display (map (lambda (c) (char-set-contains? char-set:digit c)) '(#\a #\8)))
  (display (char-set-contains? char-set:digit #\0))
  (newline))
;; Output: (#f #t) #t

(let ()
  (import (scheme base)
          (scheme write)
          (only (srfi 14)
                char-set char-set-size char-set-contains?))
  (display (vector-map (lambda (c) (char-set-contains? char-set:letter c))
                       (vector #\a #\8)))
  (display (char-set-contains? char-set:letter #\0))
  (display (char-set-size (char-set #\a #\b #\c)))
  (newline))
;; Output: #(#t #f) #f 3
