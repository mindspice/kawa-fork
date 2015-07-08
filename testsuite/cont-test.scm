(require "test-utils.scm")

(test-begin "Continuations")

(test 42 (call/cc (lambda (k)
		      (+ (k 42) 1000))))


(define incr #f)

(+ (call/cc
    (lambda (k)
      (set! incr k)
      0))
   1)

(test 6 (incr 5))
(test 8 (incr 7))
(test 10 (incr 9))


(define saved #f)
(define saved-res #f)
(define (save-test)
  (let ((x 0))
    (call/cc
     (lambda (k)
       (set! saved k)))
    (set! x (+ x 1))
    (set! saved-res x)))

(save-test)
(test 1 saved-res)
(saved)
(test 2 saved-res)
(define other saved)
(save-test)
(test 1 saved-res)
(other)
(test 3 saved-res)
(saved)
(test 2 saved-res)


(define count-before-0
  (lambda (ls)
    (call/cc
      (lambda (exit)
        (let f ((lst ls)
		(counter 0))
          (cond
            ((null? lst) counter)
            ((= (car lst) 0) (exit counter))
            (else (f (cdr lst) (+ counter 1)))))))))

(test 5 (count-before-0 '(1 2 3 4 5)))
(test 7 (count-before-0 '(7 3 8 2 1 9 5)))
(test 3 (count-before-0 '(7 3 8 0 1 9 5)))

(define contains
  (lambda (x ls)
    (call/cc
      (lambda (exit)
        (let loop ((lst ls))
	  (cond
	   ((null? lst) #f)
	   ((equal? x (car lst)) (exit #t))
	   (else (loop (cdr lst)))))))))

(test #f (contains 'd '(a b c)))
(test #t (contains 'b '(a b c)))

(define (make-iterator lst)

  (define state
    (lambda (return)
      (for-each
       (lambda (element)
	 (set! return (call/cc
		       (lambda (resume)
			 (set! state resume)
			 (return element)))))

       lst)
      (return 'end)))

  (lambda () (call/cc state)))

(define gen (make-iterator '(0 1 2)))

(test 0 (gen))
(test 1 (gen))
(test 2 (gen))
(test 'end (gen))

(define (fun)
  (call/cc (lambda (x) x)))

(define (test-fun)
  (let ((y (fun)))
    (y (lambda (x) 5))))
(test 5 (test-fun))


(define r #f)
(define result #f)
(do ((y 10)
	   (acc 1 (* y acc)))
	  ((= y 1) (set! result acc))
	(set! y (call/cc (lambda (k)
			   (set! r k)
			   (- y 1)))))
(test 362880 result)
(r 1)
(test 362880 result)
(r 2)
(test 725760 result)

;; The following tests are a modified version of some
;; examples by Pascal Rigaux (see
;; http://rigaux.org/language-study/various/callcc/scheme.html),
;; included with the author's permission
(define (inv v)
  (call/cc (lambda (return)
    (when (= v 0) (return 0))
    (/ 1 v))))
(test 0 (inv 0))
(test (/ 1 5) (inv 5))

;; Note
;;
;; From the standard:
;;  At the outermost level of a program, (begin
;;  hexpression or definition 1 i . . . ) is equivalent to the
;;  sequence of expressions and definitions in the begin.
;;
;; Inside a function body this code would loop forever.
(begin
  (define label-here1 #f)
  (call/cc (lambda (k) (set! label-here1 k)))
  (label-here1 "unused argument\n"))

(define (goto continuation) (continuation continuation))

(begin
  (define label-here (call/cc (lambda (k) k)))
  (goto label-here))

(define (label)
  (call/cc (lambda (cc) cc)))
(define (goto1 label)
  (label label))
(define c 0)
(test 5 (let ((l (label)))
	    (set! c (+ c 1))
	    (if (< c 5)
		(goto1 l)
		c)))

(define (listindex1 e l)
  (call/cc (lambda (exit)
    (letrec ((loop
	      (lambda (l)
		(cond
		 ((null? l) (exit #f))
		 ((equal? e (car l)) 0)
		 (else (+ 1 (loop (cdr l))))))))
      (loop l)))))
(test 4 (listindex1 'e '(a b c d e f g)))

(define (listindex2 e l)
  (call/cc (lambda (exit)
    (let loop ((l l))
      (cond
       ((null? l) (exit #f))
       ((equal? e (car l)) 0)
       (else (+ 1 (loop (cdr l)))))))))
(test 4 (listindex2 'e '(a b c d e f g)))

(define (generate use-it)
  (let loop ((i 0))
    (when (< i 10)
      (begin (use-it i)
	     (loop (+ i 1))))))

(define (generator->list generator)
  (call/cc (lambda (k-main)
	     (generator
	      (lambda (e)
		(call/cc (lambda (k-reenter)
			   (let ((k-main-c k-main))
			     (k-main-c (cons e (call/cc (lambda (k-new-main)
							(set! k-main k-new-main)
							(k-reenter))))))))))
	     (k-main '())
	     )))

(test '(0 1 2 3 4 5 6 7 8 9) (generator->list generate))
;; end of the Pascal Rigaux's examples

(test-end)
