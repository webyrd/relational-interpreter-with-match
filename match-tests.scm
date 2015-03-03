(load "interp-match.scm")
(load "test-check.scm")


(test "match-0"
  (run* (q) (eval-expo '(match 5) '() q))
  '())

(test "match-1a"
  (run* (q) (eval-expo '(match 5 [5 6]) '() q))
  '(6))

(test "match-1b"
  (run* (q) (eval-expo '(match 5 [,x 6]) '() q))
  '(6))

(test "match-1c"
  (run* (q) (eval-expo '(match 5 [,x x]) '() q))
  '(5))

(test "match-1d"
  (run* (q) (eval-expo '(match 5 [5 6] [7 8]) '() q))
  '(6))

(test "match-1e"
  (run* (q) (eval-expo '(match 5 [,x 6] [,y 7]) '() q))
  '(6))

(test "match-1f"
  (run* (q) (eval-expo '(match 5 [,x 6] [,x 7]) '() q))
  '(6))



(test "match-2"
  (run* (q) (eval-expo '(match (cons 5 6) [(,x . ,y) 7]) '() q))
  '(7))

(test "match-3"
  (run* (q) (eval-expo '(match (cons 5 6) [(,x . ,y) x]) '() q))
  '(5))

(test "match-4"
  (run* (q) (eval-expo '(match (cons 5 6) [(,x . ,y) y]) '() q))
  '(6))

(test "match-5"
  (run* (q) (eval-expo '(match (cons 5 6) [7 8]) '() q))
  '())

(test "match-6"
  (run* (q) (eval-expo '(match 4 [7 8]) '() q))
  '())

(test "match-7"
  (run* (q) (eval-expo '(match '(lambda (y) (y z)) [(lambda (,x) ,body) (cons x body)]) '() q))
  '((y y z)))

(test "match-8"
  (run* (q) (eval-expo '(match '((lambda (y) (y z)) 5) [(,rator ,rand) (cons rator (cons rand '()))]) '() q))
  '(((lambda (y) (y z)) 5)))

(test "match-9"
  (run* (q) (eval-expo
              '(match '((lambda (y) (y z)) 5)
                 [(lambda (,x) ,body) (cons 'lambda-expr (cons x (cons body '())))]
                 [(,rator ,rand) (cons 'app-expr (cons rator (cons rand '())))])
              '()
              q))
  '((app-expr (lambda (y) (y z)) 5)))

(test "match-10"
  (run* (q) (eval-expo
              '(match '(lambda (y) (y z))
                 [(lambda (,x) ,body) (cons 'lambda-expr (cons x (cons body '())))]
                 [(,rator ,rand) (cons 'app-expr (cons rator (cons rand '())))])
              '()
              q))
  '((lambda-expr y (y z))))

(test "match-11"
  (run* (q) (eval-expo
              '(match '(5 6 7)
                 [(,x ,y ,z) (cons 'first (cons x (cons y (cons z '()))))]
                 [(,u ,v ,w) (cons 'second (cons u (cons v (cons w '()))))])
              '()
              q))
  '((first 5 6 7)))

(test "match-12"
  (run* (q) (eval-expo
              '(match '(5 6 7)
                 [(,x ,y ,x) (cons 'first (cons x (cons y (cons x '()))))]
                 [(,u ,v ,w) (cons 'second (cons u (cons v (cons w '()))))])
              '()
              q))
  '((second 5 6 7)))

(test "match-13"
  (run* (q) (eval-expo
              '(match '(5 6 7)
                 [(,x ,y ,x) (cons 'first (cons x (cons y (cons x '()))))]
                 [(,x ,y ,z) (cons 'second (cons x (cons y (cons z '()))))])
              '()
              q))
  '((second 5 6 7)))

(test "match-14"
  (run* (q) (eval-expo
              '(match '(5 6 5)
                 [(,x ,y ,z) (cons 'first (cons x (cons y (cons z '()))))]
                 [(,u ,v ,w) (cons 'second (cons u (cons v (cons w '()))))])
              '()
              q))
  '((first 5 6 5)))

(test "match-15"
  (run* (q) (eval-expo
              '(match '(5 6 5)
                 [(,x ,y ,x) (cons 'first (cons x (cons y (cons x '()))))]
                 [(,u ,v ,w) (cons 'second (cons u (cons v (cons w '()))))])
              '()
              q))
  '((first 5 6 5)))

(test "match-16"
  (run* (q) (eval-expo
              '(match '(5 6 5)
                 [(,x ,y ,x) (cons 'first (cons x (cons y (cons x '()))))]
                 [(,x ,y ,z) (cons 'second (cons x (cons y (cons z '()))))])
              '()
              q))
  '((first 5 6 5)))




(test "match-1a-backwards"
  (run* (q) (eval-expo `(match 5 [,q 6]) '() '6))
  '(5
    (,_.0 (sym _.0))))

(test "match-1c-backwards"
  (run* (q) (eval-expo `(match 5 [,q x]) '() 5))
  '(,x))

(test "match-8-backwards"
  (run* (q)
    (eval-expo
      `(match '((lambda (y) (y z)) 5) [,q (cons rator (cons rand '()))])
      '()
      '((lambda (y) (y z)) 5)))
  '((,rator ,rand)
    ((,rator ,rand unquote _.0)
     (=/= ((_.0 cons)) ((_.0 quote)) ((_.0 rand)) ((_.0 rator)))
     (sym _.0))))

(test "match-8-backwards-verify-a"
  (run* (q)
    (eval-expo
      '(match '((lambda (y) (y z)) 5) [(,rator ,rand unquote _.0) (cons rator (cons rand '()))])
      '()
      q))
  '(((lambda (y) (y z)) 5)))

(test "match-8-backwards-verify-b"
  (run* (q)
    (eval-expo
      '(match '((lambda (y) (y z)) 5) [(,rator ,rand unquote _.0) (cons rator (cons rand '()))])
      '()
      q))
  '(((lambda (y) (y z)) 5)))

(test "match-8-backwards-verify-c"
  (run* (q)
    (eval-expo
      '(match '((lambda (y) (y z)) 5) [(,rator ,rand unquote foo) (cons rator (cons rand '()))])
      '()
      q))
  '(((lambda (y) (y z)) 5)))

(test "match-8-backwards-verify-d"
  (run* (q)
    (eval-expo
      '(match '((lambda (y) (y z)) 5) [(,rator ,rand . (unquote foo)) (cons rator (cons rand '()))])
      '()
      q))
  '(((lambda (y) (y z)) 5)))

(test "match-8-backwards-verify-e"
  (run* (q)
    (eval-expo
      '(match '((lambda (y) (y z)) 5) [(,rator ,rand . ,foo) (cons rator (cons rand '()))])
      '()
      q))
  '(((lambda (y) (y z)) 5)))



(test "eval-expo-1"
  (run* (q) (eval-expo '5 '() q))
  '(5))

(test "eval-expo-2"
  (run* (q) (eval-expo 'x '() q))
  '())

(test "eval-expo-3"
  (run* (q) (eval-expo '(lambda (x) x) '() q))
  '((closure x x ())))

(test "eval-expo-4"
  (run* (q) (eval-expo '((lambda (x) x) 5) '() q))
  '(5))

(test "eval-expo-5"
  (run* (q) (eval-expo '((lambda (x) (lambda (y) x)) 5) '() q))
  '((closure y x ((x . 5)))))



(test "quine-1"
  (run 4 (q) (eval-expo q '() q))
  '((_.0 (num _.0))
    (((lambda (_.0)
        (cons _.0 (cons (cons 'quote (cons _.0 '())) '())))
      '(lambda (_.0)
         (cons _.0 (cons (cons 'quote (cons _.0 '())) '()))))
     (=/= ((_.0 closure)) ((_.0 cons)) ((_.0 quote)))
     (sym _.0))
    (((lambda (_.0)
        (cons _.0
              (cons (cons 'quote (cons _.0 '()))
                    ((lambda (_.1) '()) _.2))))
      '(lambda (_.0)
         (cons _.0
               (cons (cons 'quote (cons _.0 '()))
                     ((lambda (_.1) '()) _.2)))))
     (=/= ((_.0 closure)) ((_.0 cons)) ((_.0 lambda))
          ((_.0 quote)) ((_.1 quote)))
     (num _.2) (sym _.0) (absento (closure _.1)))
    (((lambda (_.0)
        (cons _.0
              (cons (cons 'quote (cons _.0 '()))
                    (match _.1 (_.1 '()) . _.2))))
      '(lambda (_.0)
         (cons _.0
               (cons (cons 'quote (cons _.0 '()))
                     (match _.1 (_.1 '()) . _.2)))))
     (=/= ((_.0 closure)) ((_.0 cons)) ((_.0 match))
          ((_.0 quote)))
     (num _.1) (sym _.0) (absento (closure _.2)))))
