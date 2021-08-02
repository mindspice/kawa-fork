(import (srfi 1)
        (srfi 64))

(define-simple-class
  MyParameterAnnotatedClass ()
  (@MyAnnotType name: "class-annotation")

  ((noParams) :: String "ok")

  ((singleParamNoAnnotation
     (a1 :: String)) ::String
   "ok")

  ((singleParamSingleAnnotation
     (a2 (@MyAnnotType name: "name") :: String)) ::String
   "ok")

  ((singleParamMultipleAnnotation
     ;put one annotation after the type to test
     (a3 (@MyAnnotType name: "name1") :: String (@MyAnnotType name: "name2"))) ::String
   "ok")

  ((multiParamEachSingleAnnotation
     (a4 (@MyAnnotType name: "name1") :: String)
     (b (@MyAnnotType name: "name2") :: String)) ::String
   "ok")

  ((multiParamVariousAnnotationsCount
     (a :: String)
     (b (@MyAnnotType name: "name1") :: String)
     (c (@MyAnnotType name: "name2") (@MyAnnotType name: "name3") :: String)
     (d :: String)) ::String
   "ok"))

;; returns a vector of by-index corresponding parameter annotations list
;; where each annotation is represented by name attribute value
(define (get-param-annotations procname)
   (define methods ::object[] (invoke MyParameterAnnotatedClass:class 'getDeclaredMethods))
   (define methods-lst
     (map (lambda (m ::java.lang.reflect.Method)
            m)
          methods))
   (define method ::java.lang.reflect.Method
     (find (lambda (m ::java.lang.reflect.Method)
             (equal? procname (m:getName)))
           methods-lst))
   (vector-map
     (lambda (annotations)
       (map
         (lambda (annotation ::MyAnnotType)
           (annotation:name))
         annotations))
     (method:getParameterAnnotations)))

(test-begin "Annotations on parameter")

(test-equal
  #()
  (get-param-annotations "noParams"))

(test-equal
  #(())
  (get-param-annotations "singleParamNoAnnotation"))

(test-equal
  #(("name"))
  (get-param-annotations "singleParamSingleAnnotation"))

(test-equal
  #(("name1" "name2"))
  (get-param-annotations "singleParamMultipleAnnotation"))

(test-equal
  #(("name1") ("name2"))
  (get-param-annotations "multiParamEachSingleAnnotation"))

(test-equal
  #(() ("name1") ("name2" "name3") ())
  (get-param-annotations "multiParamVariousAnnotationsCount"))

(test-end)
