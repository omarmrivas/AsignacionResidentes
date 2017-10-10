; declare len as an uninterpreted function
(declare-fun len ((List String)) Int)

(define-const alumno String "hol")

;(define-const itis (List String) ("hol" "hol"))
(define-const itis (List String))

; assert defining equations for len as an axiom
(assert (forall ((xs (List String)))
          (ite (= nil xs)
               (= 0                     (len xs))
               (= (+ 1 (len (tail xs))) (len xs)))))

; declare append as an uninterpreted function
(declare-fun append ((List String) (List String)) (List String))

; assert defining equations for append as an axiom
(assert (forall ((xs (List String)) (ys (List String)))
            (ite (= nil xs)
                 (= (append xs ys) ys)
                 (= (append xs ys) (insert (head xs) (append (tail xs) ys))))))

; declare some existential constants
(declare-fun x () String)
(declare-fun xs () (List String))
(declare-fun ys () (List String))

; prove len (insert x xs) = 1 + len xs
; note that we assert the negation, so unsat means the theorem is valid
(push)
(assert (not (= (+ 1 (len xs)) (len (insert x xs)))))
(check-sat)
(pop)