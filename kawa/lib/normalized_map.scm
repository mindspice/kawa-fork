(module-compile-options full-continuations: #t)

(require <kawa.lib.prim_syntax>)
(require <kawa.lib.std_syntax>)
(require <kawa.lib.lists>)

(define map/cc
  (case-lambda
    ((f l) (map1 f l))
    ((f l . rest) (apply mapn f (cons l rest)))))

(define (map1 f l)
  (if (null? l)
      '()
      (let ((res (cons (f (car l)) '())))
        (map-acc f (cdr l) res)
        res)))

(define (map-acc f l acc)
  (when (not (null? l))
      (let ((t (cons (f (car l)) '())))
        (set-cdr! acc t)
        (map-acc f (cdr l) t))))

(define (mapn f . l)
  (if (null? (car l))
      '()
      (let ((res (cons (apply f (map1 car l)) '())))
        (apply mapn-acc res f (map1 cdr l))
        res)))

(define (mapn-acc acc f . l)
  (when (not (null? (car l)))
      (let ((t (cons (apply f (map1 car l)) '())))
        (set-cdr! acc t)
        (apply mapn-acc t f (map1 cdr l)))))

(define for-each/cc
  (case-lambda
    ((f l) (for-each1 f l))
    ((f l . rest) (apply for-eachn f (cons l rest)))))

(define (for-each1 f l)
  (let loop ((head (car l))
	     (tail (cdr l)))
    (f head)
    (when (not (null? tail))
      (loop (car tail)
	    (cdr tail)))))

(define (for-eachn f l . ls)
  (if (null? ls)
      (for-each1 f l)
      (begin (apply map f l ls)
	     #!void)))
