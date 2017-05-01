#lang play
(require "machine.rkt")
(print-only-errors #t) 
;;;;;;;;;;;;;;;;;;;;;;;
;; Language definition
;;;;;;;;;;;;;;;;;;;;;;;

#|
<s-expr> ::= <num>
         | <id>
         | {+ <s-expr> <s-expr>}
         | {with {<s-expr> : <type> <s-expr>} <s-expr>}
         | {fun {<id>  : <s-expr>} [: <type>] <expr>}
         | {<expr> <expr>}         
 
<type> ::= Num
         | {<type> -> <type>}}
|#
(deftype Expr
  (num n)
  (add l r)
  (id s) 
  (fun id targ body tbody)
  (fun-db body)
  (acc n) ;Se usa para la pregunta 3
  (app fun-id arg-expr))

(deftype Type
  (TNum)
  (TFun arg ret))


(define (parse-type s-expr)
  (match s-expr
    ['Num (TNum)]
    [(list arg '-> ret ) (TFun (parse-type arg) (parse-type ret))]
    [else (error "Parse error")]
    )
  )

(define (parse s-expr)
  (match s-expr
    [(? number?) (num s-expr)]
    [(list '+ l r) (add (parse l) (parse r))]
    
    [(list 'with (list id ': idtype idval) body) (app (fun id (parse-type idtype)
                                                      (parse body) #f)
                                                 (parse idval))]

    [(? symbol?) (id s-expr)]
    [(list 'fun (list id ': idtype) ': bodytype body) (fun id (parse-type idtype) (parse body) (parse-type bodytype))]
    [(list 'fun (list id ': idtype) body) (fun id (parse-type idtype) (parse body) #f)]
    [(list l r) (if (or (equal? l '+) (equal? l 'fun) (equal? l 'with))
                    (error "Parse error")
                    (app (parse l) (parse r)))]
    
 ) )





(define (typeof expr)
    (match expr
     [(num n) (if (number? n) (TNum) #f)]
     [(add l r) (if (equal? (typeof l) (TNum))
                    (if (equal? (typeof r) (TNum))
                        (TNum)
                        (error (string-append "Type error in expression + position 2: expected Num found "  (type2str (typeof r)))))
                    (error (string-append "Type error in expression + position 1: expected Num found " (type2str (typeof l)) )))
                    ]
     [(fun id idtype body bodytype)
      (let ([newbody (replaceFun id idtype body)])
        (if (equal? bodytype #f)
            (if (equal? newbody idtype)
                (TFun idtype newbody);;
                (if (add? body)
                    (if (and (equal? idtype (TNum)) (noFreeId newbody))
                        (TFun idtype (TNum))
                        (error (string-append "Type error in expression fun position 2: expected " (string-append (type2str idtype) (string-append " found "(type2str newbody ))))))
                  (if (equal?  idtype  (newbody))
                      (newbody)
                      (print (newbody)));(error (string-append "Type error in expression fun position 2: expected " (string-append (type2str idtype) (string-append " found "(type2str newbody ))))))
                )
                )
            
            (if (and (equal? bodytype idtype) (noFreeId newbody))
                (TFun idtype bodytype)
                (
                 error (string-append "Type error in expression fun position 1: expected " (string-append  (type2str bodytype) (string-append " found " (type2str idtype)))))
                )
            
            )
        ) ]  
      
   
     [(app l r) (if (fun? l)
                    (let ([funl (typeof l)])
                      (if (fun? r)
                          (error  (string-append "Type error in expression app position 2: expected "  (string-append (type2str (TFun-arg funl)) (string-append " found {Num -> Num}"))))
                          (if (fun? funl)
                              (if (equal? (typeof r) (TFun-arg funl))
                                  funl
                                  (error "Type error in expression app position 2: expected "  (string-append (type2str (TFun-arg funl)) (string-append " found " (type2str (typeof r))))))
                              (if (noFreeId r)
                                  funl
                                 (error (string-append "Type error: No type for identifier: " (type2str r))))
                              )
                          )
                      )
                    (error (string-append "Type error in expression app position 1: expected {Num -> Num} found " (type2str l)))
                    )]
      [else (error (string-append "Type error: No type for identifier: " (type2str expr)))]
 ))




(define (noFreeId expr)
  (match expr
    [(id x) (error (string-append "Type error: No type for identifier: " (type2str expr)))]
    [(TFun l r) (and (noFreeId l) (noFreeId r))]
    [TNum #t]
    ))

(define (replaceFun idx idtype body)
  (match body
    [(id x) (if (equal? x idx) idtype body)]
    [(add l r) (if (and (equal? TNum (replaceFun idx idtype l))
                        (equal? TNum (replaceFun idx idtype r)))
                   TNum
                   (TFun (replaceFun idx idtype l) (replaceFun idx idtype r)) ) ]


               ;(TFun (replaceFun idx idtype l) (replaceFun idx idtype r))]
    [(fun idx2 idtype2 body2 bodytype2) (if (equal? idx idx2)
                                            (if (equal? (replaceFun idx2 idtype2 body2 ) bodytype2 )
                                                (TFun idtype (replaceFun idx2 idtype2 body2))
                                                (TFun idtype (replaceFun idx idtype body2)))
                                            (if (equal? (typeof body) idtype )
                                                (TFun idtype (replaceFun idx2 idtype2 body2)) (print (typeof body)))
                                            )]
    [(num n) TNum]
   [(app l r) (if (fun? l)
                  (if (equal? (fun-targ l) (typeof r))
                      (replaceFun idx idtype r)
                      "error?")
                  "error")
    ]
    ;(replaceFun idx idtype r)] ;?
    )
  )

 
(define (type2str type)
  (match type
    [(TNum) "Num"]
    [(TFun l r) (string-append "{"(string-append (type2str l)  (string-append " -> " (string-append (type2str l) "}"))))]
    [(id x) (symbol->string x)]
    [TNum "Num"]
   ))

(define (symbol-append l r)
  (let ([left (if (symbol? l) (symbol->string l) l )]
        [right (if (symbol? r) (symbol->string r) r )])
  (string->symbol (string-append left right ))))


(define (type2sym type)
  (match type
    [(TNum) 'Num]
    [(TFun l r)  (cons (type2sym l)  (cons '-> (cons (type2sym l) '())))]

   ))


(define (typecheck s-expr)
  (type2sym (typeof (parse s-expr))))






(deftype Env
  (mtEnv)
  (aEnv id env))




(define (lookup id env n)
  (if (mtEnv? env)
       (error (string-append "Free identifier: " (symbol->string id)))
       (if (equal? id (aEnv-id env))
                     n
                     (lookup id (aEnv-env env) (+ n 1))
                     ))
  )



(define (isId id env n)
  (if (mtEnv? env)
       #f
       (if (equal? id (aEnv-id env))
                     n
                     (isId id (aEnv-env env) (+ n 1))
                     )))

(define (getenv expr env)
  (match expr
    [(fun id idtype body bodytype) (if (isId id env 0)
                                       (aEnv '() env)
                                       (aEnv id env))]
    [else env]
))  

    
(define (setAcc expr env)
  (match expr
     [(num n) expr]
     [(add l r) (add (setAcc  l env) (setAcc r env))]
     [(fun id idtype body bodytype) (fun id idtype (setAcc body (aEnv id env)) bodytype)]
     [(app l r) (app (setAcc  l env) (setAcc  r (getenv l env)))]


     [(id x) (acc (lookup x env 0))]
    [else expr]


    )

 )

(define (DBacc expr)
  (match expr
     [(num n) expr]
     [(add l r) (add (DBacc l) (DBacc r))]
     [(fun id idtype body bodytype)  (fun-db (DBacc body) )        ]
     [(app l r) (app (DBacc l) (DBacc r))]
     [(acc n) expr]
     [else expr]
    )
  )



(define (deBruijn expr)
   (DBacc (setAcc expr (mtEnv)) )
  )



;(define (deBruijn expr)#f)
;(define (compile expr) #f)
;(define (typeof expr)
;(define (typecheck s-expr) #f)
;(define (typed-compile s-expr) #f)


(deftype pila
  (mtPila)
  (aPila expr npila))

;C(n) = INT-CONST n
;C({+ a1 a2}) = C(a2);C(a1);ADD
;C({f a}) = C(a); C(f); APPLY
(define (expr2mach expr pila)
  (match expr
    [(num n)  (aPila (INT-CONST n) pila)]
    [(acc n)  (aPila (ACCESS n) pila)]
    [(add l r)  (expr2mach r (expr2mach l (aPila (ADD) pila )))]
    
    [(app l r)  (expr2mach r (expr2mach l (aPila (APPLY) pila ))     )]
    [(fun-db body) (aPila (CLOSURE (expr2mach body (aPila (RETURN) (mtPila))))
                          pila)]

    )
  )

(define (desApilar pila)
  (match pila
    [(mtPila) '()]
    [(aPila (CLOSURE b) npila)  (cons (CLOSURE (desApilar b)) (desApilar npila)) ]
    [(aPila expr npila) (cons expr (desApilar npila))]
    [(CLOSURE b) (cons (CLOSURE (desApilar b)) '()) ]
   ))

(define (compile expr)
   (desApilar (expr2mach expr (mtPila))) 
  )

(define (typed-compile s-expr)
  (let* ([parsed (parse s-expr)]
         [tyof (typeof parsed)]
         [tych (typecheck s-expr)]
         )
    (machine  (compile (deBruijn parsed)))
  ))




;(test (typed-compile '{with {x : Num 5}  {with  {x : Num  {+ x 1}} {+ x x}}}) 12) ; fun position 2: expected Num found {Num -> Num}"
;(test (typed-compile '{with {x : Num 5}  {with  {y : Num  {+ x 1}} {+ y x}}} ) 11) ;fun position 2: expected Num found {Num -> Num}
;(test (typed-compile 'a ) 1)
;(test/exn (typed-compile 'a ) "")
;(test (typed-compile '{fun {x : Num} x}) "")   ; (compile (deBruijn (parse  '{fun {x : Num} x})  ))





;; parse-type
(test (parse-type 'Num) (TNum))
(test (parse-type '{Num -> Num}) (TFun (TNum) (TNum)))
(test (parse-type '{{Num -> Num} -> Num}) (TFun (TFun (TNum) (TNum)) (TNum)) )
(test (parse-type '{{Num -> Num} -> {Num -> Num}}) (TFun (TFun (TNum) (TNum)) (TFun (TNum) (TNum))) )
(test (parse-type '{Num -> {Num -> {Num -> Num}}}) (TFun (TNum) (TFun (TNum) (TFun (TNum) (TNum)))) )
(test (parse-type '{Num -> {{Num -> Num} -> Num}}) (TFun (TNum) (TFun (TFun (TNum) (TNum)) (TNum))) )

(test/exn (parse-type '{}) "Parse error")
(test/exn (parse-type '{ -> Num}) "Parse error")
(test/exn (parse-type '{Num -> Num -> Num}) "Parse error")
(test/exn (parse-type '{{Num -> {Num -> Num}} -> {{{ -> Num} -> Num} -> Num}}) "Parse error")
      
;; parse
(test (parse '{+ 1 3})            (add (num 1) (num 3)))
(test (parse '{+ 1 {+ 1 1}})      (add (num 1) (add (num 1) (num 1))))
(test (parse '{+ {+ 1 1} 1})      (add (add (num 1) (num 1)) (num 1) ))

(test (parse '{with {y : Num 2} {+ x y}})   (app (fun 'y (TNum) (add (id 'x) (id 'y)) #f)(num 2)))
(test (parse '{with {x : Num 5} {+ x 3}})  (app (fun 'x (TNum)(add (id 'x) (num 3)) #f)  (num 5)))
(test (parse '{with {y : Num 3} {+ 1 {+ y 1}}})  (app (fun 'y (TNum) (add (num 1) (add (id 'y) (num 1))) #f)  (num 3)))

(test (parse '{fun {x : Num} : Num {+ x 1}}) (fun 'x (TNum) (add (id 'x) (num 1)) (TNum)) )
(test (parse '{fun {x : Num} : Num 6}) (fun 'x (TNum)  (num 6) (TNum)) )
(test (parse '{fun {x : Num} : Num {+ x x}}) (fun 'x (TNum) (add (id 'x) (id 'x)) (TNum)) )
(test (parse '{{fun {x : Num} : Num {+ x x}} 5}) (app (fun 'x (TNum) (add (id 'x) (id 'x)) (TNum)) (num 5)))
(test (parse '{{fun {x : Num} : Num {+ x x}} {fun {x : Num} : Num 5}})  (app (fun 'x (TNum) (add (id 'x) (id 'x)) (TNum)) (fun 'x (TNum) (num 5) (TNum))))
(test (parse '{fun {x : Num} : Num {+ 2 3}}) (fun 'x (TNum) (add (num 2) (num 3)) (TNum)))
(test (parse '{fun {f : {Num -> Num}} : {Num -> Num} {fun {g : Num} : Num 10}}) (fun 'f (TFun (TNum) (TNum)) (fun 'g (TNum) (num 10) (TNum)) (TFun (TNum) (TNum))))

(test/exn  (parse '{+ 1})     "Parse error")
(test/exn  (parse '{with {y : Num 2} })  "Parse error")
(test/exn  (parse '{fun 1})     "Parse error")


;; typeof
(test (typeof (parse '{+ 1 3})) (TNum))
(test (typeof (parse '{+ {+ 1 2} 4})) (TNum))
(test (typeof (parse '{+ {+ 1 2} {+ 4 5}})) (TNum))

(test (typeof (parse '{fun {x : Num} : Num 5}))  (TFun (TNum) (TNum)) )
(test (typeof (parse '{fun {x : Num} : Num {+ 2 3}})) (TFun (TNum) (TNum)))



(test (typeof (parse '{fun {x : Num} x}))  (TFun (TNum) (TNum)) )
(test (typeof (parse '{with {x : Num 5} x}))  (TFun (TNum) (TNum)) )

(test (typeof (parse '{{fun {x : Num} : Num {+ 2 3}} 5}) ) (TFun (TNum) (TNum)))          
(test (typeof (parse '{with {x : Num 5} {+ 2 3}}) ) (TFun (TNum) (TNum)))

(test (typeof (parse '{{fun {x : Num} : Num {+ 2 x}} 5}) ) (TFun (TNum) (TNum)))                        
(test (typeof (parse '{with {x : Num 5} {+ 2 x}}) ) (TFun (TNum) (TNum)))


(test (typeof (parse '{with {n : Num 4} {{fun {x : Num} {+ x n}} 1}})) (TNum))

(test/exn (typeof (parse '{{fun {x : Num} : Num {+ 2 y}} 5}) )
          "Type error: No type for identifier: y")
(test/exn (typeof (parse '{with {x : Num 5} {+ 2 y}}) )
          "Type error: No type for identifier: y")



(test/exn (typeof (parse '{fun {x : Num} : {Num -> Num} 10}))
  "Type error in expression fun position 1: expected {Num -> Num} found Num" )
(test/exn (typeof (parse '{{fun {x : Num} : Num {+ x x}} {fun {x : Num} : Num 5}}))
  "Type error in expression app position 2: expected Num found {Num -> Num}" )
(test/exn (typeof (parse '{Num {fun {x : Num} : Num 3}}))
  "Type error in expression app position 1: expected {Num -> Num} found Num" )
(test/exn (typeof (parse 'y))
  "Type error: No type for identifier: y"  )

(test/exn (typeof (parse '{+ 2 {fun {x : Num} : Num x}}))  "Type error in expression + position 2: expected Num found {Num -> Num}")

;typecheck
(test (typecheck '3) 'Num)
(test (typecheck  '{+ 2 2}) 'Num )
(test (typecheck  '{fun {f : Num} : Num 10}) '{Num -> Num})
(test (typecheck '{{fun {x : Num} : Num {+ 2 1}} 5}) '(Num -> Num))
(test (typecheck '{{fun {x : Num} : Num {+ 2 x}} 5})  '(Num -> Num))
(test (typecheck '{{fun {x : Num} : Num {+ x x}} 5}) '(Num -> Num))
(test (typecheck '{{fun {x : Num} : Num {+ x x}} {+ 2 3}}) '(Num -> Num))
(test (typecheck '{fun {x : Num} : Num {+ x x}}) '(Num -> Num))

(test (typecheck '{{fun {x : Num} : Num {+ 3 {+ x {+ 5 {+ 2 x}}}}} 5})  '(Num -> Num))
(test (typecheck  '{fun {f : {Num -> Num}} : {Num -> Num} {fun {g : Num} : Num 10}}) '((Num -> Num) -> (Num -> Num)))

(test (typecheck '{with {x : Num 5} {+ x 3}}) '(Num -> Num))
(test (typecheck '{with {x : Num 5} x}) '(Num -> Num))

(test/exn (typecheck  '{+ 2 {fun {x : Num} : Num x}})  "Type error in expression + position 2: expected Num found {Num -> Num}")
(test/exn (typecheck '{fun 1})     "Parse error")
(test/exn (typecheck '{with {x : Num 5} {+ 2 y}})          "Type error: No type for identifier: y")
(test/exn (typecheck '{{fun {x : Num} : Num {+ x 2}} y})  "Type error: No type for identifier: y")
(test/exn (typecheck '{{fun {x : Num} : Num {+ x y}} 5})  "Type error: No type for identifier: y")
(test/exn (typecheck '{fun {x : Num} : {Num -> Num} 10})  "Type error in expression fun position 1: expected {Num -> Num} found Num" )


;; deBruijn
(test (deBruijn (num 3)) (num 3))
(test (deBruijn  (parse '{+ 1 {with {x : Num 1}  {with {y : Num 2}   {+ x y}}}}))
      (add(num 1) (app(fun-db  (app (fun-db (add (acc 1) (acc 0))) (num 2))) (num 1))))
      
(test (deBruijn (parse '{with {x : Num 5}  {with  {y : Num  {+ x 1}} {+ y x}}}))
      (app (fun-db (app (fun-db (add (acc 0) (acc 1))) (add (acc 1) (num 1)))) (num 5))) ;aaa


(test (deBruijn (parse '{with {x : Num 5}  {with  {x : Num  {+ x 1}} {+ x x}}}))
      (app (fun-db (app (fun-db (add (acc 0) (acc 0))) (add (acc 1) (num 1)))) (num 5)))

(test (deBruijn (parse '{with {x : Num 5}  {with  {x : Num  {+ x 1}} {+ x x}}}))
      (app (fun-db (app (fun-db (add (acc 0) (acc 0))) (add (acc 1) (num 1)))) (num 5))) ;aaa2

(test  (deBruijn (parse '{{fun {x : Num} : Num {+ 2 1}} 5})) (app (fun-db (add (num 2) (num 1))) (num 5)))
(test  (deBruijn (parse '{{fun {x : Num} : Num {+ 2 x}} 5})) (app (fun-db (add (num 2) (acc 0))) (num 5)))
(test  (deBruijn (parse '{{fun {x : Num} : Num {+ x x}} {+ 2 3}})) (app (fun-db (add (acc 0) (acc 0))) (add (num 2) (num 3))))
(test  (deBruijn (parse '{{fun {x : Num} : Num {+ 3 {+ x {+ 5 {+ 2 x}}}}} 5}))  (app (fun-db (add (num 3) (add (acc 0) (add (num 5) (add (num 2) (acc 0)))))) (num 5)))


(test (deBruijn (parse '{with {x : Num 5} x})) (app (fun-db (acc 0)) (num 5)))
(test (deBruijn (parse '{with {x : Num 5} {+ x 3}})) (app (fun-db (add (acc 0) (num 3))) (num 5)))

(test/exn (deBruijn (parse 'x)) "Free identifier: x")

;; compile
(test (compile (deBruijn (parse '{{fun {x : Num} : Num  {+ x 10}} {+ 2 3}})))
      (list
       (INT-CONST 3)
       (INT-CONST 2)
       (ADD)
       (CLOSURE (list (INT-CONST 10) (ACCESS 0) (ADD) (RETURN)))
       (APPLY)) )

(test (compile (deBruijn (parse '3))) (list (INT-CONST 3)))

;;typed-compile
(test (typed-compile '{+ 1 3}) 4)
(test (typed-compile '{+ {+ 1 1} 1}) 3)
(test (typed-compile '{with {x : Num 5} {+ x 3}} ) 8)
(test (typed-compile '{{fun {x : Num} : Num {+ 2 1}} 5}) 3)
(test (typed-compile '{with {x : Num 5} {+ 2 x}}) 7)
(test (typed-compile  '{{fun {x : Num} : Num {+ 2 3}} 5})  5) 
(test (typed-compile  '{{fun {x : Num} : Num {+ 2 x}} 5} ) 7)
(test (typed-compile  '{{fun {x : Num} : Num {+ 2 x}} {+ 2 3}} ) 7)
(test (typed-compile '{with {n : Num 4} {{fun {x : Num}{+ x n}} 1}}) 5)
(test (typed-compile  '{with {x : Num 5} x}) 5)

(test (typed-compile  '{with {x : Num 5} {+ 2 3}})  5)                
(test (typed-compile  '{with {x : Num 5} {+ 2 x}}) 7)

(test/exn (typed-compile 'a ) "Type error: No type for identifier: a")
(test/exn (typed-compile '{+ 1}) "Parse error")
(test/exn (typeof (parse '{{fun {x : Num} : Num {+ 2 y}} 5}) )
          "Type error: No type for identifier: y")
(test/exn (typed-compile '{+ 2 {fun {x : Num} : Num x}})  "Type error in expression + position 2: expected Num found {Num -> Num}")















