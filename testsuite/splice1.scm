(define xa [3 4 5])
(define oarr (object[] 5 4 3))

(define l1 (list @xa 9 @xa))
(format #t "l1: ~w~%" l1)
;; Output: l1: (3 4 5 9 3 4 5)

(format #t "integer[]-1: ~w ~w~%"  (integer[] 66 @oarr 9 @xa) 99)
;; Output: integer[]-1: [66 5 4 3 9 3 4 5] 99

(format #t "int[]-2: ~w ~w~%"  (int[] 66 @xa 9 @oarr) 99)
;; Output: int[]-2: [66 3 4 5 9 5 4 3] 99

(format #t "sum-xa: ~w~%" (+ @xa))
;; Output: sum-xa: 12

(format #t "sum-oarr: ~w~%" (+ @oarr))
;; Output: sum-oarr: 12

(format #t "sum-xb: ~w~%" (+ 100 @xa @xa 13))
;; Output: sum-xb: 137
