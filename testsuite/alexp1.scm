;; A snippet extracted from "alexpander.scm".
;; This is a best for GitLab issues #11:
;; "attempting to push dead variable" error on loading Alexpander

(let ()

(define (expand-any sexp store ek sk dk-ea) ;D/377/fl:40084; L62/fl:1
  (define (handle-syntax-use syntax head) ;L/72/fl:b
    (if (symbol? syntax)
        (dk-ea syntax sexp store 0)
        sexp))
  (cond ((symbol? sexp)
         (sk (cdr (assv sexp store)) sexp))
        (else
         (expand-any (car sexp) store
                     #t handle-syntax-use #f))))

(define (expand-val sexp store k-ev)
  (expand-any sexp store
              (lambda (output)
                (k-ev store output))
              (lambda (syn) (k-ev syn store))
              list))

(define (dk-et builtin sexp store loc-n)
  (expand-val 
   sexp
   store
   (lambda (val store) loc-n)))

(define builtins-store '((define-syntax . define-syntax)))

(define null-prog-1
  '(define-syntax letrec-syntax
     (let-syntax ((let-syntax let-syntax) (define-syntax define-syntax))
       (syntax-rules ()
         ((_ ((kw init) ...) . body)
          (let-syntax ()
            (define-syntax kw init) ... (let-syntax () . body)))))))

(define null-stuff (expand-any null-prog-1 builtins-store #f #f dk-et))

(write (equal? (format #f "~a" null-stuff) "(define-syntax (define-syntax letrec-syntax (let-syntax ((let-syntax let-syntax) (define-syntax define-syntax)) (syntax-rules () ((_ ((kw init) ...) . body) (let-syntax () (define-syntax kw init) ... (let-syntax () . body)))))) ((define-syntax . define-syntax)) 0)"))
(newline)
;; Output: #t
)
