;; https://savannah.gnu.org/bugs/?52390
(let ((v (map (lambda (prefix)
                (object (java.lang.Object)
                        ((my-method value)
                         (string-append prefix "-" (number->string value)))))
              (list "ALPHA" "BETA" "GAMMA"))))
  (format #t "~a ~a ~a : test-map~%~!"
          @(map (lambda (vx i) ((dynamic invoke) vx 'my-method i))
                v '(100 200 300))))
;; Output:  ALPHA-100 BETA-200 GAMMA-300 : test-map
