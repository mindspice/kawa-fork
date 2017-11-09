(defun car (x)
  (if x
      (invoke (the KAWA:pair x) '|getCar|)
      nil))

(defun first (x)
  (car x))

(defun cdr (x)
  (if x
      (invoke (the pair x) '|getCdr|)
      nil))

(defun rest (x)
  (cdr x))

(defun second (x)
  (first (rest x)))

(defun third (x)
  (first (rest (rest x))))

(defun nthcdr (n lst)
  (KAWA:declare (KAWA:int n))
  (do ((i n (1- i))
       (result lst (cdr result)))
      ((= i 0) result)))

(defun nth (n x)
  (first (nthcdr n x)))

(defun 1- (x) (- x 1))
(defun 1+ (x) (+ x 1))

(defun acons (key datum alist)
  (cons (cons key datum) alist))

(defun listp (obj)
  (typep obj 'list))

(defun numberp (obj)
  (typep obj 'KAWA:number))

(defun atom (obj)
  (not (consp obj)))

(defun eql (x y)
  (eqv? x y))

(defun complement (pred)
  (lambda (&rest arguments)
    (not (apply pred arguments))))

(defun member-with-test (x lst test key)
  (declare (list lst))
  (cond ((null lst) nil)
	((funcall test x (funcall key (car lst))) lst)
	(t (member-with-test x (cdr lst) test key))))

(defun member-with-key (x lst key)
  (declare (list lst))
  (cond ((null lst) nil)
	((eql x (funcall key (car lst))) lst)
	(t (member-with-key x (cdr lst) key))))

(defun member-plain (x lst)
  (declare (list lst))
  (cond ((null lst) nil)
	((eql x (car lst)) lst)
	(t (member-plain x (cdr lst)))))

(defun member (x lst &key key
		       (test nil test-supplied)
		       (test-not nil test-not-supplied))
  (declare (list lst))
  (cond (test-supplied
	 (member-with-test x lst test key))
	(test-not-supplied
	 (member-with-test x lst (complement test-not) key))
	(key
	 (member-with-key x lst key))
	(t
	 (member-plain x lst))))

(defun funcall (func &rest args)
  (apply func args))

(defun minusp (x)
  (< x 0))

(defun plusp (x)
  (> x 0))

;; ANSI: This should be inclosed in "an implicit block whose name is
;; the function block name of the function-name or name, as
;; appropriate." But we don't have support for CL blocks yet.
(define-syntax flet
  (syntax-rules ()
    ((_ ((fname parameters body ...) ...)
	e ...)
     (%flet ((fname (lambda parameters body ...)) ...)
	    e ...))))

(define-syntax labels
  (syntax-rules ()
    ((labels ((fname parameters body ...) ...) e ...)
     (flet ((fname parameters #!void) ...)
        (set! #'fname (lambda parameters body ...)) ...
	e ...))))

;; This is a hack. The calling conventions of Kawa will need to
;; be adjusted to make this conform to ANSI CL.
;; To wit (t-woo)
;; (multiple-value-bind (x) (values 1 2 3) (list x)) ;=> <error>. In
;; fact, passing an improper number of arguments to a continuation
;; implicitly accepting a single value is undefined in Scheme.
;;
;; The workaround of course is to provide superfluous arguments,
;; (multiple-value-bind (x y z) (values 1 2 3) (list x)) ;=> (1)
(define-syntax multiple-value-bind
  (syntax-rules ()
    ((_ parameters producer body ...)
     (call-with-values (lambda () producer)
       (lambda parameters body ...)))))

(defun floor (number &optional (divisor 1))
  (values (div number divisor) (remainder number divisor)))

(defun keywordp (x)
  (invoke-static "gnu.kawa.lispexpr.LispPackage" "keywordp" x))

(defun symbol-package (symbol)
  ;; FIXME: type decl not working yet
  ;; (declare (type symbol symbol))
  (invoke-static "gnu.kawa.lispexpr.LispPackage" "symbolPackage" symbol))

;; Coerce a string-designator to a string
(defun %to-string (x)
  (cond ((stringp x) x)
	;; FIXME: use characterp
	((typep x 'KAWA:character) (string x))
	(t (symbol-name x))))

(defun find-package (name)
  ;;(declare (type (or string symbol character package) name))
  (if (packagep name)
      name
      (let ((pkg (invoke-static "gnu.kawa.lispexpr.LispPackage" "findPackage"
				(as "String" (%to-string name)))))
	(if (eq pkg #!null) nil pkg))))

;; Convert a package designator to a package
(defun %to-package (x)
  (or (find-package x)
      (error "Invalid package designator" x)))

(defun intern (name &optional (pkg *package*))
  ;;(declare (type string name) (type (or package string symbol) pkg))
  (invoke-static "gnu.kawa.lispexpr.LispPackage" "intern"
		 (as "String" name)
		 (%to-package pkg)))

(defun packagep (x) (typep x 'KAWA:gnu.kawa.lispexpr.LispPackage))

(defun find-symbol (name &optional (pkg *package*))
  ;;(declare (type string name) (type (or package string symbol) pkg))
  (invoke (as "gnu.kawa.lispexpr.LispPackage" (%to-package pkg)) "findSymbol"
	  (as "String" name)))

(define-syntax multiple-value-list
  (syntax-rules ()
    ((_ form) (call-with-values (lambda () form) (lambda (&rest list) list)))))

(define-syntax nth-value
  (syntax-rules ()
    ((_ n form) (nth n (multiple-value-list form)))))

(define-syntax progn
  (syntax-rules ()
    ((_) nil)
    ((_ form ...) (begin form ...))))

(define-syntax when
  (syntax-rules ()
    ((_ test form ...) (if test (progn form ...) nil))))

(define-syntax unless
  (syntax-rules ()
    ((_ test form ...) (if test nil (progn form ...)))))

(define-syntax dolist
  (syntax-rules ()
    ((_ (var list) body ...)
     (let loop ((l list))
	  (cond ((null l) nil)
		(t (let ((var (car l)))
		     body ...)
		   (loop (cdr l))))))))

(defun mapcar (fun list) (map fun list))

(defun make-package (name &key nicknames use)
  (let* ((name (%to-string name))
	 (nicknames (mapcar #'%to-string nicknames)))
    (dolist (n (cons name nicknames))
      (when (find-package n)
	(error "A package with that name arleady exists" n)))
    (invoke-static "gnu.kawa.lispexpr.LispPackage" "valueOf"
		   (as "String" name))))

;; Convert a list-of-symbols designator to a list-of-symbols
(defun %to-list-of-symbols (x)
  (cond ((symbolp x) (list x))
	((listp x) x)
	(t (error "Not a list-of-symbol designator" x))))

(defun export (symbols &optional (pkg *package*))
  (invoke-static "gnu.kawa.lispexpr.LispPackage" "exportPkg"
		 (%to-list-of-symbols symbols)
		 (%to-package pkg))
  t)

