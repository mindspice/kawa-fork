;;; Procedures for implementing SRFI-25 arrays.

(require <kawa.lib.prim_imports>)
(require <kawa.lib.std_syntax>)
(require kawa.lib.exceptions)

(define-alias <array>  <gnu.lists.Array>)

(define (array? x) :: <boolean>
  (instance? x <array>))

(define (shape #!rest (args :: <Object[]>)) :: <array>
  (invoke-static <gnu.kawa.functions.Arrays> 'shape args))

(define (make-array (shape :: <array>) #!rest (obj ::object[])) :: <array>
  (invoke-static <gnu.kawa.functions.Arrays> 'makeFromValues shape obj))

(define ($array_constructor$ (shape :: <array>) #!rest (vals :: <Object[]>))
  (invoke-static <gnu.kawa.functions.Arrays> 'makeSimple
		 shape (gnu.lists.FVector vals)))

(define (->shape (shape ::array))::array
  (! srank (shape:rank))
  (cond ((and (= srank 2) (= (shape:getSize 1) 2))
         shape)
        ((= srank 1)
         (! rank (shape:getSize 0))
         (! ivec (gnu.kawa.functions.Arrays:handleShapeSpecifier
                  shape rank #!null #t))
         (gnu.lists.GeneralArray (gnu.lists.S32Vector ivec)
                                 (int[] rank 2)
                                 #!null))
        (else
         (error "array shape must be a sequence or a rank*2 array"))))

(define (array-rank (array :: <array>)) :: <int>
  (invoke array 'rank))

(define (array-size (arr :: <array>)) :: <int>
  (arr:getSize))

(define (array-start (array :: <array>) (k :: <int>)) :: <int>
  (invoke array 'getLowBound k))

(define (array-end  (array :: <array>) (k :: <int>)) :: <int>
  (+ (invoke array 'getLowBound k) (invoke array 'getSize k)))

(define (array-shape (arr :: array)) ::array
  (let* ((rank (array-rank arr))
         (iarr (int[] length: (* 2 rank)))
         (ivec (gnu.lists.S32Vector iarr))
         (result (gnu.lists.GeneralArray ivec (int[] rank 2) #!null)))
    (do ((i ::int 0 (+ i 1)) (i2 ::int 0 (+ i2 2)))
        ((>= i rank)
         (ivec:setReadOnly)
         result)
      (set! (iarr i2) (array-start arr i))
      (set! (iarr (+ i2 1)) (array-end arr i)))))

(define (share-array (array :: <array>) (shape :: <array>)
		     (mapper :: <procedure>))
  (invoke-static  <gnu.kawa.functions.Arrays> 'shareArray array shape mapper))

(define (array-index-ref (arr ::<array>) #!rest (indexes ::object[]))
  (gnu.lists.ComposedArray:generalIndex arr #f @indexes)) ;; FIXME should inline

(define (array-index-share (arr ::<array>) #!rest (indexes ::object[]))
  (gnu.lists.ComposedArray:generalIndex arr #t @indexes)) ;; FIXME should inline

(define (array-flatten (arr ::<array>))
  (gnu.lists.Arrays:flattenCopy arr #t))

(define (array->vector (arr ::<array>))
  (gnu.lists.FlattenedArray:flatten arr))

(define (index-array (shape ::<array>)) ::<array>
  (let ((arr (gnu.kawa.functions.Arrays:allocateArray shape)))
    (arr:setBase gnu.lists.Range:zeroAndUp)
    arr))

(define (array-copy! dst::<array> src::<array>)::void
   (gnu.lists.Arrays:copy dst src))

(define (array-fill! arr::<array> value)::void
  (gnu.lists.Arrays:fill arr value))

(define (array-transform arr::<array> shape::<array> mapper::procedure)::<array>
  (gnu.kawa.functions.Arrays:getTransformed arr mapper shape))

(define (build-array shape::<array> getter::procedure
                     #!optional (setter::procedure #!null)) ::<array>
   (gnu.kawa.functions.Arrays:getBuiltArray shape getter setter))

(define (array-reshape arr::<array> shape::<array>)::<array>
  (let* ((result (gnu.kawa.functions.Arrays:allocateArray shape))
         (vec (gnu.lists.FlattenedArray:flatten arr))
         (vsz (vec:size))
         (sz (result:getSize)))
    (if (not (= sz vsz))
        (error (format "shape requires ~d elements but argument has ~d"
                       sz vsz)))
    (result:setBase vec)
    result))

(define (format-array value #!optional (arg1 #!null) (arg2 #!null))
  (cond ((? port ::java.lang.Appendable arg1)
         (port:append (gnu.kawa.functions.ArrayPrint:print value arg2))
         #!void)
        ((eqv? arg1 #t)
         ((gnu.kawa.io.OutPort:outDefault):append
          (gnu.kawa.functions.ArrayPrint:print value arg2))
         #!void)
        ((eqv? arg1 #f)
         (gnu.kawa.functions.ArrayPrint:print value arg2))
        (else
         (gnu.kawa.functions.ArrayPrint:print value arg1))))
