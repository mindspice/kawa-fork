(require kawa.lib.parameters)
(require kawa.lib.prim_imports)
(require kawa.lib.std_syntax)
(require kawa.lib.syntax)

(define-syntax parameterize%
  (syntax-rules ()
    ((parameterize% () restore . body)
     (try-finally
      (begin . body)
      (begin . restore)))
    ((parameterize% ((param1 value1) . rest) restore . body)
     (let* ((p ::gnu.mapping.Location (as-location% param1))
	    (v value1)
	    (save (p:setWithSave v)))
       (parameterize% rest
		      ((p:setRestore save) . restore)
		      . body)))))

(define-syntax parameterize
  (syntax-rules ()
    ((parameterize () . body)
     (begin . body))
    ((parameterize ((param1 value1) . rest) . body)
     (parameterize% ((param1 value1) . rest) () . body))))
