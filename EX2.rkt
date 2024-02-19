#lang pl

;------------------------------------Q1-------------------------------------


; The square function takes a number and returns its square
(: square : Number -> Number)
(define (square x) (* x x))


; The sum-of-squares function takes a list of numbers as input and returns the sum of their squares.
; It uses the foldl function to iterate over the list and accumulate the sum of the squares of the numbers.
; If the list is empty, it throws an error.

(: sum-of-squares :(Listof Number) -> Number)
(define (sum-of-squares lst)
  (cond
   [(null? lst) (error 'sum-of-squares "the list is empty ")] ; If the list is empty, throw an error
  [else (foldl + 0 (map square lst))] ; Otherwise, use foldl to sum the squares of the numbers in the list
))

(test (sum-of-squares '(1 2 3)) => 14)
(test (sum-of-squares '(4 5 6)) => 77) ; 4^2 + 5^2 + 6^2 = 16 + 25 + 36 = 77
(test (sum-of-squares '(0 0 0)) => 0) ; 0^2 + 0^2 + 0^2 = 0
(test (sum-of-squares '(-1 -2 -3)) => 14) ; (-1)^2 + (-2)^2 + (-3)^2 = 1 + 4 + 9 = 14
(test (sum-of-squares '(7)) => 49) ; 7^2 = 49
(test (sum-of-squares '()) =error> "the list is empty ") ; Empty list should return an error

;------------------------------------Q2.a---------------------------------
(: createPolynomial : (Listof Number) -> (Number -> Number))

(define (createPolynomial coeffs)
  ;; Define a helper function 'poly' that computes the value of the polynomial at a given 'x'
  (: poly : (Listof Number) Number Integer Number -> Number)
  (define (poly argsL x power accum)
    ;; If 'argsL' is empty, return the accumulator 'accum'
    (if (null? argsL)
        accum
        ;; Else, recursively call 'poly' with the rest of 'argsL', the same 'x', an incremented 'power', and an updated 'accum'
        (poly (rest argsL) x (+ power 1) (+ accum (* (first argsL) (expt x power))))))
  ;; Define a function 'polyX' that represents the polynomial with the given coefficients
  (: polyX : Number -> Number)
  (define (polyX x)
    ;; Call 'poly' with the coefficients 'coeffs', a given 'x', a starting power of '0', and a starting accumulator of '0'
    (poly coeffs x 0 0))
  ;; Return the 'polyX' function
  polyX)

;; Define a polynomial with coefficients [2, 3, 4, 5]
(define p2345 (createPolynomial '(2 3 4 5)))
(test (p2345 0) => (+ (* 2 (expt 0 0)) (* 3 (expt 0 1)) (* 4 (expt 0 2)) (* 5 (expt 0 3))))
(test (p2345 4) => (+ (* 2 (expt 4 0)) (* 3 (expt 4 1)) (* 4 (expt 4 2)) (* 5 (expt 4 3))))
(test (p2345 11) => (+ (* 2 (expt 11 0)) (* 3 (expt 11 1)) (* 4 (expt 11 2)) (* 5 (expt 11 3))))

;; Define a polynomial with coefficients [5, 3, 6]
(define p536 (createPolynomial '(5 3 6)))
(test (p536 11) => (+ (* 5 (expt 11 0)) (* 3 (expt 11 1)) (* 6 (expt 11 2))))

;; Define a polynomial with no coefficients
(define p_0 (createPolynomial '()))
(test (p_0 4) => 0)

;-------------------------------------------------Q2.b----------------------------------------------------------


;-----------------------------i)-----------------

#|BNF
 The grammar:
   
   <PLANG>::=
          1){{poly <AEs>} <AEs>}

   <AEs>::=
        1){<AEs> <AE>}|
        2){<AE>}
        
   <AE> ::=
        1)   <num>|
        2)  {+ <AE> <AE>}|
        3)  {- <AE> <AE>}|
        4)  {* <AE> <AE>}|
        5)  {/ <AE> <AE>}|

         
          
|#


;---------------------------ii)-------------------


(define-type PLANG [Poly (Listof AE) (Listof AE)])


(define-type AE 
          [Num Number] 
          [Add AE AE] 
          [Sub AE AE] 
          [Mul AE AE] 
          [Div AE AE])


 (: parse-sexpr : Sexpr -> AE) 
 ;; to convert s-expressions into AEs 
 (define (parse-sexpr sexpr) 
             (match sexpr 
             [(number: n) (Num n)] 
             [(list '+ lhs rhs) (Add (parse-sexpr lhs) (parse-sexpr rhs))] 
             [(list '- lhs rhs) (Sub (parse-sexpr lhs) (parse-sexpr rhs))] 
             [(list '* lhs rhs) (Mul (parse-sexpr lhs) (parse-sexpr rhs))] 
             [(list '/ lhs rhs) (Div (parse-sexpr lhs) (parse-sexpr rhs))] 
             [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))

(test (parse-sexpr (string->sexpr "{+ 2 3}")) => (Add (Num 2) (Num 3)))

 
 (: parse : String -> PLANG) 
 ;; parses a string containing a PLANG expression to a PLANG AST
 (define (parse str) (let ([code (string->sexpr str)])
                     ;Here is code equivalent to the first part of parsing (String -> AST) 
                     (match code
                       [(list (cons 'poly list) '()) (error 'parse "at least one point is required in ~s" code)]
                       [(list (cons 'poly '()) (list x ...)) (error 'parse "at least one coefficient is required in ~s" code)]
                       [(list (cons 'poly param) (list points ...)) (Poly (map parse-sexpr param) (map parse-sexpr points))]
                       [else (error 'parse "bad syntax in ~s" code)]     
                       ))) 


 
 (test (parse "{{poly 1 2 3} {1 2 3}}") => (Poly (list (Num 1) (Num 2) (Num 3)) (list (Num 1) (Num 2) (Num 3)))) 
 (test (parse "{{poly } {1 2} }") =error> "parse: at least one coefficient is required in ((poly) (1 2))") 
 (test (parse "{{poly 1 2} {} }") =error> "parse: at least one point is required in ((poly 1 2) ())")
 (test (parse "{{poly {/ 4 2} {- 4 1}} {{- 8 4}}}") => (Poly (list (Div (Num 4) (Num 2)) (Sub (Num 4) (Num 1)))(list (Sub (Num 8) (Num 4)))))


;------------------------iii)-------------------

#|
;; evaluates AE expressions to numbers 
 (define (eval expr) 
   (cases expr 
     [(Num n) n] 
     [(Add l r) (+ (eval l) (eval r))] 
     [(Sub l r) (- (eval l) (eval r))] 
     [(Mul l r) (* (eval l) (eval r))] 
     [(Div l r) (/ (eval l) (eval r))]))


 (: eval-poly : PLANG -> <-fill in-> ) 
      (define (eval-poly p-expr) 
                 <-fill in-> )

 (: run : String -> (Listof Number)) 
 ;; evaluate a FLANG program contained in a string

 (define (run str) (eval-poly (parse str)))




(test (run "{{poly 1 2 3} {1 2 3}}") => '(6 17 34)) 
(test (run "{{poly 4 2 7} {1 4 9}}") => '(13 124 589)) 
(test (run "{{poly 1 2 3} {1 2 3}}") => '(6 17 34)) 
(test (run "{{poly 4/5 } {1/2 2/3 3}}") => '(4/5 4/5 4/5)) 
(test (run "{{poly 2 3} {4}}") => '(14)) 
(test (run "{{poly 1 1 0} {-1 3 3}}") => '(0 4 4))) 
(test (run "{{poly {/ 4 2} {- 4 1}} {{- 8 4}}}") => '(14)) 
(test (run "{{poly {+ 0 1} 1 {* 0 9}} {{- 4 5} 3 {/ 27 9}}}") => '(0 4 4))
|#