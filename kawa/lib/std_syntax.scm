;;; Definitions for some standard syntax.

;;; COND

(define-syntax cond
  (syntax-rules (else =>)

		((cond (else result1 result2 ...))
		 (begin result1 result2 ...))

		((cond (else result1 result2 ...) clause1 ...)
		 (%syntax-error "else clause must be last clause of cond"))

		((cond (test => result))
		 (let ((temp test))
		   (if temp (result temp))))

		((cond (test => result) clause1 clause2 ...)
		 (let ((temp test))
		   (if temp
		       (result temp)
		       (cond clause1 clause2 ...))))

		((cond (test))
		 test)

		((cond (test) clause1 clause2 ....)
		 (or test (cond clause1 clause2 ...)))

		((cond (test result1 result2 ...))
		 (if test (begin result1 result2 ...)))

		 ((cond (test result1 result2 ...) clause1 clause2 ...)
		 (if test
		     (begin result1 result2 ...)
		     (cond clause1 clause2 ...)))))

;;; CASE

(define-syntax case (syntax-rules ()
				  ((case key clauses ...)
				   (let ((tmp key))
				     (%case tmp clauses ...)))))

(define-syntax %case (syntax-rules (else)
				   ((%case key (else expression ...))
				    (begin expression ...))
				   ((%case key (else expression ...) . junk)
				    (%syntax-error
				     "junk following else (in case)"))
				   ((%case key
					   ((datum ...) expression ...))
				    (if (%case-match key datum ...)
					(begin expression ...)))
				   ((%case key
					   ((datum ...) expression ...)
					   clause more ...)
				    (if (%case-match key datum ...)
					(begin expression ...)
					(%case key clause more ...)))))
					  
(define-syntax %case-match (syntax-rules ()
					 ((%case-match key datum)
					  (eqv? key (quote datum)))
					 ((%case-match key datum more ...)
					  (or (eqv? key 'datum)
					      (%case-match key more ...)))))

;;; LET (including named let)

(define-syntax let (syntax-rules ()
		    ((let (bindings ...) body ...)
		     (%let (bindings ...) body ...))
		    ((let name ((var init) ...) body ...)
		     ((letrec ((name (lambda (var ...)
				       body ...)))
			name)
		      init ...))))


;;; LET*

(define-syntax let* (syntax-rules ()
				 ((let* () . body) (%let () . body))
				 ((let* ((var init)) . body)
				  (%let ((var init)) . body))
				 ((let* ((var init) . bindings) . body)
				  (%let ((var init))
					(let* bindings . body)))
				 ((let* bindings . body)
				  (%syntax-error
				   "invalid bindings list in let*"))
				 ((let* . body)
				  (%syntax-error
				   "missing bindings list in let*"))))

;;; DO

;;; Helper macro for do, to handle optional step.
(define-syntax %do-step (syntax-rules ()
				      ((%do-step variable step) step)
				      ((%do-step variable) variable)))

(define-syntax do (syntax-rules ()
				((do ((variable init . step) ...)
				     (test sequence) commands ...)
				 (letrec ((loop
					   (lambda (variable ...)
					     (if test
						 (begin sequence)
						 (begin
						   commands ...
						   (loop (%do-step variable . step) ...))))))
				   (loop init ...))
				 )))

;;; DELAY

(define-syntax delay (syntax-rules ()
				   ((delay expression)
				    (%make-promise (lambda () expression)))))
