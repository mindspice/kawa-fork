(define scheme :: <kawa.standard.Scheme> (kawa.standard.Scheme:getInstance))
(scheme:eval "(! x :: <java.lang.String> (java.lang.String 'foo))")
;; Here comes the crash:
(scheme:eval "(display x) (newline)")
;; Output: foo
