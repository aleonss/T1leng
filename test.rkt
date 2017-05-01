#lang play
(require "main.rkt")
(require "machine.rkt")


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






