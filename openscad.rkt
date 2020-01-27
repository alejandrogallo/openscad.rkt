; vim-run: racket -if %


(define ($fa val) (format "$fa = ~a;\n" val))
(define ($fs val) (format "$fs = ~a;\n" val))
(define ($fn val) (format "$fn = ~a;\n" val))

(define (print-csv v)
  (cond
    ((null? v) "")
    (else
      (string-append
        ; check if the first element is a number or symbol
        ; allow for variables too
        (let ((s (car v)))
          (cond
            ((number? s) (number->string s))
            ((symbol? s) (symbol->string s))
            (else s)))
        ; do not print a trailing coma
        (if (null? (cdr v)) "" ", ")
        ; recurse
        (print-csv (cdr v))))))

(define (to-vector v)
  (string-append "[" (print-csv v) "]"))

(define (number-or-list v)
  (if (number? v) (number->string v) (to-vector v)))

;; 2D
(define (circle [r 1] #:d [d (* r 2)])
  (format "circle(d=~a);\n" d))

(define (polygon points [paths '()])
  (format "polygon(~a, ~a);\n" (to-vector points) (to-vector paths)))

(define (square size center)
  (format "square(~a, ~a);\n" (number-or-list size) (to-vector center)))

;; 3D
(define (sphere [r 1] #:d [d (* r 2)])
  (format "sphere(d=~a);\n" d))

(define (cube size center)
  (format "cube(~a, ~a);\n" (number-or-list size) (to-vector center)))

;; BOLEAN OPERATIONS
(define (union fst snd)
  (format "union() {\n~a~a}\n" fst snd))
