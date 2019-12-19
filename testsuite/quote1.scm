(display 'abc:def) (newline)
;; Output: abc:def

(display 'abc:def:xyz) (newline)
;; Output: ($lookup$ ($lookup$ abc (quasiquote def)) (quasiquote xyz))

;; GitLab issue #62 "Unquoteable (this) if followed by colon notation"
(display '(this):foo) (newline)
;; Output: ($lookup$ (this) (quasiquote foo))

(display '(abs):foo) (newline)
;; Output: ($lookup$ (abs) (quasiquote foo))
