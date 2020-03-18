(module-name (srfi 48))
(module-export format)
(import (scheme base))

(define (sformat (format::java.lang.CharSequence) (args::object[]) (arg_start::int))::constant-string
  (let ((port (make gnu.kawa.io.CharArrayOutPort)))
    (rformat port format args arg_start)
    (! str (port:toString))
    (port:close)
    str))

(define (rformat port::java.lang.Appendable format::java.lang.CharSequence
                 args::object[] arg-start::int)
  ::void
  ((gnu.kawa.functions.LispFormat:asSrfi48Format format):format
   args arg-start port #!null))

(define (format arg1 #!rest args::object[])
  (cond ((string? arg1)
         (sformat arg1 args 0))
        ((boolean? arg1)
         (if arg1
             (rformat (current-output-port) (args 0) args 1)
             (sformat (args 0) args 1)))
        ((java.io.Writer? arg1)
         (rformat arg1 (args 0) args 1))
        ((java.io.OutputStream? arg1)
         (rformat arg1 (args 0) args 1))
        (else
         (error "invalid first argument for format"))))
