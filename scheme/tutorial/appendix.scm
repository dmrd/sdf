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
;; Symbols

;; Backquote

;; Effects

;; Assignments
