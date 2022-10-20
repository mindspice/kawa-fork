;; Generate char-tables.iscm which is included by srfi14.scm.
(import (kawa regex))

(define prog-name (regex-replace (regex "^.*/") (car (command-line)) ""))
(define out ::output-port (current-output-port))

(define (print-matching-codepoints predicate nl out)
  (do 
    ((i ::int #x10FFFF (- i 1)) ;; codepoint to check
     (last-matched #f) ;; if (+ i 1) was a match
     (col ::int 0)) ;; printed column; wrap every 10th number
    ((< i 0) (when last-matched (display "0")))
    (let* ((matched (predicate i))
           (print (not (boolean=? last-matched matched))))
      (set! last-matched matched)
      (when print
        (display i out)
        (set! col (modulo (+ 1 col) 10))
        (if (= 0 col)
            (display nl out)
            (display " " out))))))

(define (print-list name predicate #!optional (out (current-output-port)))
  (format out "(define-early-constant %~a ::int[]~%    (constant-fold int[] " name)
  (print-matching-codepoints predicate          "\n                         " out)
  (format out "))~%~%"))

(define-syntax define-union
  (syntax-rules ()
    ((_ name pred ...)
     (define (name codepoint ::int) ::boolean
       (or (pred codepoint) ...)))))

(define (is-blank? codepoint ::int) ::boolean
  (or (= codepoint #x0009)
      (= (java.lang.Character:get-type codepoint) java.lang.Character:SPACE_SEPARATOR)))

(define (is-whitespace? codepoint ::int) ::boolean
  ;; The set of whitespace characters is all the characters in
  ;; Unicode categories Zs, Zl or Zp, along with points 9-13.
  (or (= codepoint #x0009)
      (= codepoint #x000a)
      (= codepoint #x000b)
      (= codepoint #x000c)
      (= codepoint #x000d)
      (let ((type ::byte (java.lang.Character:get-type codepoint)))
        (or (= type java.lang.Character:SPACE_SEPARATOR)
            (= type java.lang.Character:LINE_SEPARATOR)
            (= type java.lang.Character:PARAGRAPH_SEPARATOR)))))

(define (is-punctuation? codepoint ::int) ::boolean
  (let ((type ::byte (java.lang.Character:get-type codepoint)))
    (or (= type java.lang.Character:CONNECTOR_PUNCTUATION)
        (= type java.lang.Character:DASH_PUNCTUATION)
        (= type java.lang.Character:START_PUNCTUATION)
        (= type java.lang.Character:END_PUNCTUATION)
        (= type java.lang.Character:INITIAL_QUOTE_PUNCTUATION)
        (= type java.lang.Character:FINAL_QUOTE_PUNCTUATION)
        (= type java.lang.Character:OTHER_PUNCTUATION))))

(define (is-symbol? codepoint ::int) ::boolean
  (let ((type ::byte (java.lang.Character:get-type codepoint)))
    (or (= type java.lang.Character:MATH_SYMBOL)
        (= type java.lang.Character:CURRENCY_SYMBOL)
        (= type java.lang.Character:MODIFIER_SYMBOL)
        (= type java.lang.Character:OTHER_SYMBOL))))

(define-union is-letter-or-digit? java.lang.Character:letter? java.lang.Character:digit?)

(define-union is-graphic? java.lang.Character:letter? java.lang.Character:digit? is-punctuation? is-symbol?)

(define-union is-printing? java.lang.Character:letter? java.lang.Character:digit? is-punctuation? is-symbol? is-whitespace?)


(format out ";; This -*- scheme -*- file is generated by ~a.  DO NOT EDIT.~%"
        prog-name)

(print-list "title-case"   java.lang.Character:title-case?)
(print-list "whitespace"   is-whitespace?)
(print-list "blank"        is-blank?)
(print-list "lower-case"   java.lang.Character:lower-case?)
(print-list "upper-case"   java.lang.Character:upper-case?)
(print-list "letter"       java.lang.Character:letter?)
(print-list "digit"        java.lang.Character:digit?)
(print-list "punctuation"  is-punctuation?)
(print-list "symbol"       is-symbol?)
(print-list "letter+digit" is-letter-or-digit?)
(print-list "graphic"      is-graphic?)
(print-list "printing"     is-printing?)
