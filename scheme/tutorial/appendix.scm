; (operator operand-1 ... operand-n)
(+ 1 2.14)

; Lambda functions
((lambda (x) (* x x)) 4)

; Equivalent
(define square (lambda (x) (* x x)))
(square 4)

; Equivalent with syntactic sugar
(define (square2 x) (* x x))
(square2 4)

(define pi 3.141592653589793)
(* pi (square 4))

(define compose
  (lambda (f g)
    (lambda (x)
    (f (g x)))))

((compose square square) 4)

; Using syntactic sugar
(define ((compose2 f g) x) (f (g x)))

((compose2 square square) 4)

; Using function defined inside
(define (compose3 f g)
  (define (fog x)
    (f (g x)))
  fog
  )

((compose3 square square) 4)

;; Conditionals

; Define abs
(define (abs x)
  (cond
   ((< x 0) (- x))
   ((= x 0) (x))
   ((> x 0) (x))
        ))
(abs -5)
(abs 5)

(define (abs2 x)
  (cond
   ((< x 0) (- x))
   (else x)
        ))
(abs2 -5)
(abs2 5)

(define (abs3 x)
  (if (< x 0)
      (- x)
      x))
(abs3 -5)
(abs3 5)


;; Recursive functions
;;
(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))
(factorial 10)


;; Local names: let expressions
(define (volume_to_area radius)
  ; Can have several clauses within the let
  (let ((area (* 4 pi (square radius)))
        (volume (* 4/3 pi (cube radius)))
        )
    (/ volume area)
    ))
(volume_to_area 5)



;; Data types
(define a-list (list 6 946 8 356 12 620))
a-list


(list? a-list)
(list? 3) ; => false

(define a-pair (cons 1 2))
a-pair

(car a-pair)
(cdr a-pair)

(cons a-pair a-list)

(define a-vector (vector 1 2 3))

a-vector

(vector-ref a-vector 2)

;; Record types
(define-record-type point
  (make-point x y)
  point?
  (x point-x)
  (y point-y))

(define p (make-point 1 2))
(point? p)
(point-x p)
(point-y p)


;; Variable # of arguments

(define (accumulate proc initial lst)
  (if (null? lst)
      initial
      (proc (car lst)
            (accumulate proc initial (cdr lst))
            )
      )
  )

(accumulate + 0 '(1 2 3))

; cheating a bit since -:unary & -:binary aren't actually defined
(define (-:unary x) (- x))
(define (-:binary x y) (- x y))

; The `.` means the rest of the arguments after the first go to ys
; (my- 1 2 3 4) --> x = 1, y = '(1 2 3)
(define (my- x . ys)
  (if (null? ys)
      (-:unary x)
      (-:binary x (accumulate + 0 ys))))
(my- 10)
(my- 10 1 2 3)

;; TODO
;;
;; Symbols
;; Test whether 2 symbols are identical
(eq? 'a 'a)
(eq? 'a 'b)

(define (mul? expression)
  (and (pair? expression)
       (eq? (car expression '*)))
  )

(mul? '(* 2 4))

(mul? '(+ 2 4))


;; Backquote
;; Quasiquoting allows evaluating elements within a quoted expression
;; backtick ` for quasiquote
;; comma before
(let ((name 'a)) `(list ,name ',name)) ;; (list a 'a)
(let ((name 'a)) '(list ,name ',name)) ;; (list (unquote name) (quote (unquote name)))

; Can splice lists together with ,@ operator
;
`(a b ,@(list (+ 20 3) (- 20 3)) d) ; (a b 23 17 d)


;; Effects
;; Side effect-ful functions
(write-line 5)
(write-line "Print out")

; Final block of a let statement is the return value.
(let ((name "David"))
  (write-line name)
  name
  )


;; Assignments
;; Good practice to avoid assignments
;; Mutable variables

(define (make-counter)
  ; Introduce variable to mutate in scope
  (let ((count 0))
    (lambda ()
      ; Set the value in the closure
      (set! count (+ count 1))
      count)))
(define c1 (make-counter))
(define c2 (make-counter))
(c1) ; 1
(c1) ; 2

(c2) ; 1
(c2) ; 2

; Can assign to elements of data structures
;
(define pair '(1 2))
pair ; 1 2
(set-car! pair 5)
pair ; 5 2
(set-cdr! pair 10)
pair ; 5 10
(set-cdr! pair (list 1 2 3))
pair ; 5 1 2 3

(set-car! pair (list 1 2 3))
pair ; (1 2 3) 1 2 3


; lists
(define index 2)
(define new-value 100)

(define lst (list 1 2 3))
(list-set! lst index new-value)
lst ; (1 2 100)

(define vec (vector 1 2 3))
(vector-set! vec index new-value)
vec ; #(1 2 100)

; Without setters
(define-record-type point-unsettable
  (make-point x y)
  point?
  (x point-x)
  (y point-y))

; with setters
(define-record-type settable-point
  (make-settable-point x y)
  point?
  (x point-x set-x!)
  (y point-y set-y!))

(define p (make-settable-point 1 2))
(point-x p) ; 1
(set-x! p 3)
(point-x p) ; 3
