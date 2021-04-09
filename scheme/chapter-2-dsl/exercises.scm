;; 2.1:

; Associate arity with a function
(define (restrict-arity proc nargs)
  (hash-table-set! arity-table proc nargs)
  proc)

; Get arity of a function
(define (get-arity proc)
  (or (hash-table-ref/default arity-table proc #f)
      (let ((a (procedure-arity proc))) ;arity not in table
        (assert (eqv? (procedure-arity-min a)
                      (procedure-arity-max a)))
        (procedure-arity-min a))))

; The lookup table from function -> arity
(define arity-table (make-key-weak-eqv-hash-table))

; Basic compose - No checking
(define (compose_v1 f g)
  (define (the-composition . args)
    (call-with-values (lambda () (apply g args))
      f))
  (restrict-arity the-composition (get-arity g)))

                                        ;
; Compose with arity checking
; TODO: Is this actually desirable? Probably requires knowing # of return values
; from g
(define (compose_v2 f g)
  (let (
        (nf (get-arity f))
        (ng (get-arity g))
        )
    (assert (= nf ng))
    (define (the-composition . args)
      ; Check that we are given right number of arguments to begin with
      (assert (= (length args) nf))
      (call-with-values (lambda () (apply g args))
        f))
    (restrict-arity the-composition ng)
    ))

(define (square a) (* a a))
(define squaresquare_fail (compose_v2 square square_both))

(define squaresquare (compose_v2 square square))
(squaresquare 5)



; Parallel function application
; (define (square_both a b) '((square a) (square b)))
; (define squaresquare_both (compose square square_both))
;
; (get-arity square_both)
;
; (get-arity squaresquare_both)
;
;
; ; Base parallel-combine
; (define (simple-parallel-combine h f g)
;   (define (the-combination . args)
;     (h (apply f args) (apply g args)))
;   (let ((n1 (get-arity f))
;         (n2 (get-arity g)))
;     (assert (= n1 n2))
;     (restrict-arity the-combination n1)))
