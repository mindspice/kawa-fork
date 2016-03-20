(require <kawa.lib.characters>)
(require <kawa.lib.strings>)
(require <kawa.lib.kawa.expressions>)
(require <kawa.lib.ExceptionClasses>)
(define-alias MultValuesType gnu.kawa.reflect.MultValuesType)
(define-alias OccurrenceType gnu.kawa.reflect.OccurrenceType)

(define-validate pipeProcessValidateApply (exp required proc)
  ((= exp:arg-count 2)
   (exp:visitArgs (get-visitor))
   (let ((e0 (exp:getArg 0))
         (e1 (exp:getArg 1))
         (visitor (get-visitor)))
     (if (and (? ae1 ::gnu.expr.ApplyExp e1)
              (eq? (ae1:function:valueIfConstant)
                   gnu.kawa.functions.RunProcess:instance))
         (let* ((ae1 ::gnu.expr.ApplyExp e1)
                (aeargs ae1:args))
           (apply-exp ae1:function in: e0 @aeargs))
         (visitor:error #\e "pipe-process arg not run-process" e1)))))

(define-validate charToIntegerValidateApply (exp required proc)
  ((exp:isSimple 1 1)
   (let ((e0 (visit-exp (exp:getArg 0) character)))
     (apply-exp as int
                (apply-exp gnu.kawa.functions.Convert:cast character e0)))))

(define-validate integerToCharValidateApply (exp required proc)
  ((exp:isSimple 1 1)
   (apply-exp as character
              (apply-exp gnu.kawa.functions.Convert:cast int (exp:getArg 0)))))

(define-validate isEofValidateApply (exp required proc)
  ((exp:isSimple 1 1)
   (exp:visitArgs (get-visitor))
   (let* ((e0 (exp:getArg 0))
          (t0 (e0:getType)))
     (cond ((or (eq? t0 character) (eq? t0 character-or-eof))
            (apply-exp = (apply-exp as int e0) -1))
           ((gnu.kawa.reflect.LazyType:maybeLazy t0)
            (apply-exp eq? (apply-exp gnu.mapping.Promise:force e0) #!eof))
           (else #!null)))))

(define-validate charCompareValidateApply (exp required proc)
  ((exp:isSimple)
   (define name proc:name)
   (define n exp:arg-count)
   (define num-op
     (cond ((or (name:equals "char=?") (name:equals "char-ci=?")) =)
           ((or (name:equals "char<?") (name:equals "char-ci<?")) <)
           ((or (name:equals "char>?") (name:equals "char-ci>?")) >)
           ((or (name:equals "char<=?") (name:equals "char-ci<=?")) <=)
           ((or (name:equals "char>=?") (name:equals "char-ci>=?")) >=)
           (else #!null)))
   (cond ((eq? num-op #!null)
          exp)
         (else
          (define ci (> (name:indexOf "ci") 0))
          (do ((i::int 0 (+ i 1)))
              ((= i n))
            (let ((e (apply-exp char->integer (exp:getArg i))))
              (if ci
                  (set! e (apply-exp invoke-static java.lang.Character 'toUpperCase e)))
              (exp:setArg i e)))
          (apply-exp num-op @exp:args)))))

(define-validate stringCursorCompareValidateApply (exp required proc)
  ((exp:isSimple 2 2)
   (define name proc:name)
   (define n exp:arg-count)
   (define num-op
     (cond ((name:equals "string-cursor=?") =)
           ((name:equals "string-cursor<?") <)
           ((name:equals "string-cursor>?") >)
           ((name:equals "string-cursor<=?") <=)
           ((name:equals "string-cursor>=?") >=)
           (else #!null)))
   (cond ((eq? num-op #!null)
          #!null)
         (else
          (do ((i::int 0 (+ i 1)))
              ((= i n))
            (let ((e (apply-exp as int (exp:getArg i))))
              (exp:setArg i e)))
          (apply-exp num-op @exp:args)))))

(define-validate stringAppendToValidateApply (exp required proc)
  ((exp:isSimple 2 2)
   (exp:visitArgs (get-visitor))
   (let* ((e0 (exp:getArg 0))
          (e1 (exp:getArg 1))
          (t1 (e1:getType))
          (compat-character (invoke character 'isCompatibleWithValue t1))
          (compat-char (invoke char 'isCompatibleWithValue t1))
          (compat-string (invoke string 'isCompatibleWithValue t1)))
     (cond ((and (> compat-character 0) (<= compat-string 0))
            (apply-exp invoke (apply-exp as gnu.lists.FString e0) 'appendCharacter (apply-exp char->integer e1)))
           ((and (> compat-char 0) (<= compat-string 0))
            (apply-exp invoke (apply-exp as gnu.lists.FString e0) 'append (apply-exp as char e1)))
           ((and (> compat-string 0) (< compat-character 0))
            (apply-exp invoke (apply-exp as gnu.lists.FString e0) 'append (apply-exp as java.lang.CharSequence e1)))
           (else
            (apply-exp invoke (apply-exp as gnu.lists.FString e0) 'append e1)))))
  ((exp:isSimple 3)
   (define comp (get-compilation))
   (comp:letStart)
   (define seqDecl (comp:letVariable #!null gnu.lists.FString (exp:getArg 0)))
   (define nargs (exp:getArgCount))
   (comp:letEnter)
   (comp:letDone
    (begin-exp 
     @(let loop ((i ::int 1) (stmts ()))
        (if (= i nargs) (reverse (cons #!void stmts))
            (loop (+ i 1)
                  (cons (apply-exp string-append! seqDecl (exp:getArg i))
                        stmts))))))))

(define-validate valuesValidateApply (exp required proc)
  ((exp:isSimple 1 1)
   (visit-exp (exp:getArg 0) required))
  ((exp:isSimple)
   (let* ((args exp:args)
          (arg-count args:length)
          (arg-count-ok
           (= (OccurrenceType:compatibleWithCount required arg-count) 0))
          (rmult ::MultValuesType
                 (if (and arg-count-ok (MultValuesType? required))
                     required
                     #!null))
          (rtypes (gnu.bytecode.Type[] length: arg-count)))
     (do ((i ::int 0 (+ i 1)))
         ((= i arg-count))
       (let ((e (visit-exp (args i)
                           (if (eq? rmult #!null) #!null
                               (rmult:getValueType i)))))
         (set! (rtypes i) (e:getType))
         (set! (args i) e)))
     (exp:setType (MultValuesType:create rtypes))
     exp)))

(define-validate raiseValidateApply (exp required proc)
  ((exp:isSimple 1 1)
   (apply-exp primitive-throw
              (apply-exp invoke-static ExceptionWithValue 'wrap (exp:getArg 0)))))

(define (valuesCompile exp::gnu.expr.ApplyExp comp::gnu.expr.Compilation
                       target::gnu.expr.Target proc::gnu.mapping.Procedure)
  ::boolean
  (define pproc ::gnu.expr.PrimProcedure 
    (if (exp:isSimple 2 2)
        (gnu.expr.PrimProcedure 
         (invoke gnu.expr.Compilation:typeValues 'getDeclaredMethod
                 "values2" 2)
         exp:type (gnu.bytecode.Type[] Object Object))
        (gnu.expr.PrimProcedure 
         (invoke gnu.expr.Compilation:typeValues 'getDeclaredMethod
                 "makeFromArray" 1))))
  (pproc:setReturnType exp:type)
  (define ae::gnu.expr.ApplyExp (apply-exp pproc @exp:args))
  (ae:compile comp target)
  #t)

(define-validate lengthValidateApply (exp required proc)
  ((exp:isSimple 1 1)
   (let* ((arg (exp:getArg 0))
          (atype (arg:getType)))
     (cond ((atype:isSubtype (Type:make java.util.List))
            (apply-exp invoke arg 'size))
           ((atype:isSubtype (Type:make java.lang.CharSequence))
            (apply-exp invoke arg 'length))
           ((gnu.bytecode.ArrayType? atype)
            (apply-exp field arg 'length))
           (else
            (apply-exp gnu.lists.Sequences:getSize arg))))))
