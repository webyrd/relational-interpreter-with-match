;; The version of the relational interpreter in
;; 'interp-with-variadic-lambda-and-match.scm' supports 'apply', variadic
;; 'lambda'/application, multi-argument 'lambda'/application, and a
;; fair number of built-ins, such as 'quote', 'list', and 'cons'.
;; And 'match'!
;;
;; Importantly, 'apply' has been moved towards the top of the 'conde'
;; in 'eval-expo', ensuring that the answers will contain many uses of
;; 'apply'.  In general, to get more answers containing a form or
;; primitive function, move the form towards the top of the 'conde' in
;; 'eval-expo' (and vice versa to de-emphasize a form).  The ordering
;; of the 'conde' clauses give us some crude control over how
;; miniKanren explores the search space of terms.
(load "interp-with-variadic-lambda-and-match.scm")
(load "mk/test-check.scm")
(load "mk/matche.scm")

;; Helper Scheme predicate for testing
(define member? (lambda (x ls) (not (not (member x ls)))))

(test "env-match-1"
  (run* (q)
    (eval-expo
     '((lambda (w)
         (match '(lambda (y) (y z))
           [`(lambda (,x) ,body) (cons w body)]))
       6)
      '()
      q))
  '((6 y z)))


(test "Scheme-interpreter-1"
  (run* (q)
    (eval-expo
     `(letrec ((eval-expr
                (lambda (expr env)
                  (match expr
                    [(? symbol? x) (env x)]
                    [`(lambda (,(? symbol? x)) ,body)
                     (lambda (a)
                       (eval-expr body (lambda (y)
                                         (if (equal? x y)
                                             a
                                             (env y)))))]
                    [`(,rator ,rand)
                     ((eval-expr rator env) (eval-expr rand env))]))))
        (eval-expr '(lambda (z) z) (lambda (y) ((lambda (z) z)))))
     '()
     q))
  '((closure
     (lambda (a)
       (eval-expr body
                  (lambda (y) (if (equal? x y) a (env y)))))
     (ext-env body z
              (ext-env x z
                       (ext-env env
                                (closure (lambda (y) ((lambda (z) z)))
                                         (ext-rec
                                          ((eval-expr
                                            (lambda (expr env)
                                              (match expr ((? symbol? x) (env x))
                                                     (`(lambda (,(? symbol? x)) ,body)
                                                      (lambda (a)
                                                        (eval-expr body
                                                                   (lambda (y)
                                                                     (if (equal? x y) a (env y))))))
                                                     (`(,rator ,rand)
                                                      ((eval-expr rator env)
                                                       (eval-expr rand env)))))))
                                          ()))
                                (ext-env expr (lambda (z) z)
                                         (ext-rec
                                          ((eval-expr
                                            (lambda (expr env)
                                              (match expr ((? symbol? x) (env x))
                                                     (`(lambda (,(? symbol? x)) ,body)
                                                      (lambda (a)
                                                        (eval-expr body
                                                                   (lambda (y)
                                                                     (if (equal? x y) a (env y))))))
                                                     (`(,rator ,rand)
                                                      ((eval-expr rator env)
                                                       (eval-expr rand env)))))))
                                          ()))))))))

(test "Scheme-interpreter-2a"
  (run* (q)
    (eval-expo
      `(letrec ((eval-expr
                 (lambda (expr env)
                   (match expr
                     [(? number? n) n]
                     [(? symbol? x) (env x)]
                     [`(lambda (,(? symbol? x)) ,body)
                      (lambda (a)
                        (eval-expr body (lambda (y)
                                          (if (equal? x y)
                                              a
                                              (env y)))))]
                     [`(,rator ,rand)
                      ((eval-expr rator env) (eval-expr rand env))]))))
         (eval-expr '5 (lambda (y) ((lambda (z) z)))))
      '()
      q))
  '(5))

(test "Scheme-interpreter-2b"
  (run 5 (q)
    (eval-expo
      `(letrec ((eval-expr
                 (lambda (expr env)
                   (match expr
                     [(? number? n) n]
                     [(? symbol? x) (env x)]
                     [`(lambda (,(? symbol? x)) ,body)
                      (lambda (a)
                        (eval-expr body (lambda (y)
                                          (if (equal? x y)
                                              a
                                              (env y)))))]
                     [`(,rator ,rand)
                      ((eval-expr rator env) (eval-expr rand env))]))))
         (eval-expr ',q (lambda (y) ((lambda (z) z)))))
      '()
      '5))
  '(5
    (((lambda (_.0) 5) _.1)
     (=/= ((_.0 closure)))
     (num _.1)
     (sym _.0))
    (((lambda (_.0) _.0) 5)
     (=/= ((_.0 closure)))
     (sym _.0))
    (((lambda (_.0) 5) (lambda (_.1) _.2))
     (=/= ((_.0 closure)) ((_.1 closure)))
     (sym _.0 _.1)
     (absento (closure _.2)))
    (((lambda (_.0) ((lambda (_.1) 5) _.2)) _.3)
     (=/= ((_.0 closure)) ((_.1 closure)))
     (num _.2 _.3)
     (sym _.0 _.1))))

(test "Scheme-interpreter-2b"
  (run 5 (q)
    (eval-expo
      `(letrec ((eval-expr
                 (lambda (expr env)
                   (match expr
                     [(? number? n) n]
                     [(? symbol? x) (env x)]
                     [`(lambda (,(? symbol? x)) ,body)
                      (lambda (a)
                        (eval-expr body (lambda (y)
                                          (if (equal? x y)
                                              a
                                              (env y)))))]
                     [`(,rator ,rand)
                      ((eval-expr rator env) (eval-expr rand env))]))))
         (eval-expr ,q (lambda (y) ((lambda (z) z)))))
      '()
      '5))
  '(5
    '5
    ('((lambda (_.0) 5) _.1)
     (=/= ((_.0 closure)))
     (num _.1)
     (sym _.0))
    ((match _.0 (_.0 5) . _.1)
     (num _.0))
    ('((lambda (_.0) _.0) 5)
     (=/= ((_.0 closure)))
     (sym _.0))))

(test "Scheme-interpreter-2c"
  (run* (q)
    (eval-expo
      `(letrec ((eval-expr
                 (lambda (expr env)
                   (match expr
                     [(? number? n) n]
                     [(? symbol? x) (env x)]
                     [`(lambda (,(? symbol? x)) ,body)
                      (lambda (a)
                        (eval-expr body (lambda (y)
                                          (if (equal? x y)
                                              a
                                              (env y)))))]
                     [`(,rator ,rand)
                      ((eval-expr rator env) (eval-expr rand env))]))))
         (eval-expr '((lambda (w) 5) 6) (lambda (y) ((lambda (z) z)))))
      '()
      '5))
  '(_.0))

(test "Scheme-interpreter-2d"
  (run 2 (q)
    (eval-expo
      `(letrec ((eval-expr
                 (lambda (expr env)
                   (match expr
                     [(? number? n) ,q]
                     [(? symbol? x) (env x)]
                     [`(lambda (,(? symbol? x)) ,body)
                      (lambda (a)
                        (eval-expr body (lambda (y)
                                          (if (equal? x y)
                                              a
                                              (env y)))))]
                     [`(,rator ,rand)
                      ((eval-expr rator env) (eval-expr rand env))]))))
         (eval-expr '((lambda (w) 5) 6) (lambda (y) ((lambda (z) z)))))
      '()
      '5))
  '(5
    n))

(test "Scheme-interpreter-2e"
  (run 1 (q)
    (absento 5 q)
    (absento 6 q)
    (absento 'w q)
    (absento 'lambda q)
    (eval-expo
      `(letrec ((eval-expr
                 (lambda (expr env)
                   (match expr
                     [,q n]
                     [(? symbol? x) (env x)]
                     [`(lambda (,(? symbol? x)) ,body)
                      (lambda (a)
                        (eval-expr body (lambda (y)
                                          (if (equal? x y)
                                              a
                                              (env y)))))]
                     [`(,rator ,rand)
                      ((eval-expr rator env) (eval-expr rand env))]))))
         (eval-expr '((lambda (w) 5) 6) (lambda (y) ((lambda (z) z)))))
      '()
      '5))
  '((`((,_.0 ,_.1 ,n) unquote _.2)
     (=/= ((_.0 _.1)) ((_.0 _.2)) ((_.0 lambda)) ((_.0 n))
          ((_.0 w)) ((_.1 _.2)) ((_.1 lambda)) ((_.1 n))
          ((_.1 w)) ((_.2 lambda)) ((_.2 n)) ((_.2 w)))
     (sym _.0 _.1 _.2))))

(test "Scheme-interpreter-2f"
  (run* (q)
    (eval-expo
      `(letrec ((eval-expr
                 (lambda (expr env)
                   (match expr
                     [(? symbol? x) (env x)]
                     [`(lambda (,(? symbol? x)) ,body)
                      (lambda (a)
                        (eval-expr body (lambda (y)
                                          (if (equal? x y)
                                              a
                                              (env y)))))]
                     [`(,rator ,rand)
                      ((eval-expr rator env) (eval-expr rand env))]
                     [(? number? n) n]))))
         (eval-expr '((lambda (w) 5) 6) (lambda (y) ((lambda (z) z)))))
      '()
      '5))
  '(_.0))

(test "Scheme-interpreter-2g"
  (run* (q)
    (eval-expo
      `(letrec ((eval-expr
                 (lambda (expr env)
                   (match expr
                     [(? symbol? x) (env x)]
                     [`(lambda (,(? symbol? x)) ,body)
                      (lambda (a)
                        (eval-expr body (lambda (y)
                                          (if (equal? x y)
                                              a
                                              (env y)))))]
                     [`(,rator ,rand)
                      ((eval-expr rator env) (eval-expr rand env))]
                     [,q n]))))
         (eval-expr '((lambda (w) 5) 6) (lambda (y) ((lambda (z) z)))))
      '()
      '5))
  '(n
    (? number? n)
    `,n
    `,(? number? n)))

(test "Scheme-interpreter-2h"
  (run* (q)
    (eval-expo
      `(letrec ((eval-expr
                 (lambda (expr env)
                   (match expr
                     [(? number? n) n]
                     [(? symbol? x) (env x)]
                     [`(lambda (,(? symbol? x)) ,body)
                      (lambda (a)
                        (eval-expr body (lambda (y)
                                          (if (equal? x y)
                                              a
                                              (env y)))))]
                     [`(,rator ,rand)
                      ((eval-expr rator env) (eval-expr rand env))]))))
         (cons
           (eval-expr '((lambda (w) 5) 6) (lambda (y) ((lambda (z) z))))
           (eval-expr '((lambda (v) v) 7) (lambda (y) ((lambda (z) z))))))
      '()
      '(5 . 7)))
  '(_.0))

(test "Scheme-interpreter-2h"
  (run 1 (q)
    (eval-expo
      `(letrec ((eval-expr
                 (lambda (expr env)
                   (match expr
                     [,q n]
                     [(? symbol? x) (env x)]
                     [`(lambda (,(? symbol? x)) ,body)
                      (lambda (a)
                        (eval-expr body (lambda (y)
                                          (if (equal? x y)
                                              a
                                              (env y)))))]
                     [`(,rator ,rand)
                      ((eval-expr rator env) (eval-expr rand env))]))))
         (cons
           (eval-expr '((lambda (w) 5) 6) (lambda (y) ((lambda (z) z))))
           (eval-expr '((lambda (v) v) 7) (lambda (y) ((lambda (z) z))))))
      '()
      '(5 . 7)))
  '((? number? n)))

(test "Scheme-interpreter-2i"
  (run 2 (q)
    (eval-expo
      `(letrec ((eval-expr
                 (lambda (expr env)
                   (match expr
                     [,q n]
                     [(? symbol? x) (env x)]
                     [`(lambda (,(? symbol? x)) ,body)
                      (lambda (a)
                        (eval-expr body (lambda (y)
                                          (if (equal? x y)
                                              a
                                              (env y)))))]
                     [`(,rator ,rand)
                      ((eval-expr rator env) (eval-expr rand env))]))))
         (cons
           (eval-expr '((lambda (w) 5) 6) (lambda (y) ((lambda (z) z))))
           (eval-expr '((lambda (v) v) 7) (lambda (y) ((lambda (z) z))))))
      '()
      '(5 . 7)))
  '((? number? n)
    `,(? number? n)))

;;; Does run 3 ever come back???
(test "Scheme-interpreter-2j"
  (run 2 (q)
    (eval-expo
      `(letrec ((eval-expr
                 (lambda (expr env)
                   (match expr
                     [,q n]
                     [(? symbol? x) (env x)]
                     [`(lambda (,(? symbol? x)) ,body)
                      (lambda (a)
                        (eval-expr body (lambda (y)
                                          (if (equal? x y)
                                              a
                                              (env y)))))]
                     [`(,rator ,rand)
                      ((eval-expr rator env) (eval-expr rand env))]))))
         (cons
           (eval-expr '((lambda (w) 5) 6) (lambda (y) ((lambda (z) z))))
           (eval-expr '((lambda (v) v) 7) (lambda (y) ((lambda (z) z))))))
      '()
      '(5 . 7)))
  '((? number? n)
    `,(? number? n)))

(test "Scheme-interpreter-2k"
  (run* (q)
    (eval-expo
      `(letrec ((eval-expr
                 (lambda (expr env)
                   (match expr
                     [(? number? n) n]
                     [(? symbol? x) (env x)]
                     [`(lambda (,(? symbol? x)) ,body)
                      (lambda (a)
                        (eval-expr body (lambda (y)
                                          (if (equal? x y)
                                              a
                                              (env y)))))]
                     [`(cons ,e1 ,e2)
                      (cons (eval-expr e1 env) (eval-expr e2 env))]
                     [`(,rator ,rand)
                      ((eval-expr rator env) (eval-expr rand env))]))))
         (eval-expr '(cons 5 6) (lambda (y) ((lambda (z) z)))))
      '()
      q))
  '((5 . 6)))

(test "Scheme-interpreter-2l"
  (run* (q)
    (eval-expo
      `(letrec ((eval-expr
                 (lambda (expr env)
                   (match expr
                     [(? number? n) n]
                     [(? symbol? x) (env x)]
                     [`(lambda (,(? symbol? x)) ,body)
                      (lambda (a)
                        (eval-expr body (lambda (y)
                                          (if (equal? x y)
                                              a
                                              (env y)))))]
                     [`(cons ,e1 ,e2)
                      (cons (eval-expr e1 env) (eval-expr e2 env))]
                     [`(,rator ,rand)
                      ((eval-expr rator env) (eval-expr rand env))]))))
         (eval-expr '(cons (cons 5 6) (cons 7 8)) (lambda (y) ((lambda (z) z)))))
      '()
      q))
  '(((5 . 6) . (7 . 8))))







(test "Scheme-interpreter-3"
  (run* (q)
    (eval-expo
      `(letrec ((eval-expr
                 (lambda (expr env)
                   (match expr
                     [(? number? n) n]
                     [(? symbol? x) (env x)]
                     [`(lambda (,(? symbol? x)) ,body)
                      (lambda (a)
                        (eval-expr body (lambda (y)
                                          (if (equal? x y)
                                              a
                                              (env y)))))]
                     [`(,rator ,rand)
                      ((eval-expr rator env) (eval-expr rand env))]))))
         (eval-expr '((lambda (z) z) 5) (lambda (y) ((lambda (z) z)))))
      '()
      q))
  '(5))

(test "Scheme-interpreter-4"
  (run* (q)
    (eval-expo
     `(letrec ((eval-expr
                (lambda (expr env)
                  (match expr
                    [(? number? n) n]
                    [(? symbol? x) (env x)]
                    [`(lambda (,(? symbol? x)) ,body)
                     (lambda (a)
                       (eval-expr body (lambda (y)
                                         (if (equal? x y)
                                             a
                                             (env y)))))]
                    [`(,rator ,rand)
                     ((eval-expr rator env) (eval-expr rand env))]))))
        (eval-expr '((lambda (z) z) (lambda (w) w)) (lambda (y) ((lambda (z) z)))))
     '()
     q))
  '((closure
     (lambda (a)
       (eval-expr body
                  (lambda (y) (if (equal? x y) a (env y)))))
     (ext-env body w
              (ext-env x w
                       (ext-env env
                                (closure (lambda (y) ((lambda (z) z)))
                                         (ext-rec
                                          ((eval-expr
                                            (lambda (expr env)
                                              (match expr ((? number? n) n)
                                                     ((? symbol? x) (env x))
                                                     (`(lambda (,(? symbol? x)) ,body)
                                                      (lambda (a)
                                                        (eval-expr body
                                                                   (lambda (y)
                                                                     (if (equal? x y) a (env y))))))
                                                     (`(,rator ,rand)
                                                      ((eval-expr rator env)
                                                       (eval-expr rand env)))))))
                                          ()))
                                (ext-env expr (lambda (w) w)
                                         (ext-rec
                                          ((eval-expr
                                            (lambda (expr env)
                                              (match expr ((? number? n) n)
                                                     ((? symbol? x) (env x))
                                                     (`(lambda (,(? symbol? x)) ,body)
                                                      (lambda (a)
                                                        (eval-expr body
                                                                   (lambda (y)
                                                                     (if (equal? x y) a (env y))))))
                                                     (`(,rator ,rand)
                                                      ((eval-expr rator env)
                                                       (eval-expr rand env)))))))
                                          ()))))))))

(test "Scheme-interpreter-5"
  (run* (q)
    (eval-expo
      `(letrec ((eval-expr
                 (lambda (expr env)
                   (match expr
                     [(? number? n) n]
                     [(? symbol? x) (env x)]
                     [`(lambda (,(? symbol? x)) ,body)
                      (lambda (a)
                        (eval-expr body (lambda (y)
                                          (if (equal? x y)
                                              a
                                              (env y)))))]
                     [`(,rator ,rand)
                      ((eval-expr rator env) (eval-expr rand env))]))))
         (eval-expr '((lambda (z) 6) 7) (lambda (y) ((lambda (z) z)))))
      '()
      q))
  '(6))

(test "Scheme-interpreter-6"
  (run 10 (q)
    (eval-expo
     `(letrec ((eval-expr
                (lambda (expr env)
                  (match expr
                    [(? number? n) n]
                    [(? symbol? x) (env x)]
                    [`(lambda (,(? symbol? x)) ,body)
                     (lambda (a)
                       (eval-expr body (lambda (y)
                                         (if (equal? x y)
                                             a
                                             (env y)))))]
                    [`(,rator ,rand)
                     ((eval-expr rator env) (eval-expr rand env))]))))
        (eval-expr ',q (lambda (y) ((lambda (z) z)))))
     '()
     '6))
  '(6
    (((lambda (_.0) 6) _.1)
     (=/= ((_.0 closure))) (num _.1)
     (sym _.0))
    (((lambda (_.0) _.0) 6)
     (=/= ((_.0 closure))) (sym _.0))
    (((lambda (_.0) 6) (lambda (_.1) _.2))
     (=/= ((_.0 closure)) ((_.1 closure))) (sym _.0 _.1)
     (absento (closure _.2)))
    (((lambda (_.0) ((lambda (_.1) 6) _.2)) _.3)
     (=/= ((_.0 closure)) ((_.1 closure))) (num _.2 _.3)
     (sym _.0 _.1))
    (((lambda (_.0) ((lambda (_.1) _.1) 6)) _.2)
     (=/= ((_.0 closure)) ((_.1 closure))) (num _.2)
     (sym _.0 _.1))
    (((lambda (_.0) ((lambda (_.1) 6) _.2)) (lambda (_.3) _.4))
     (=/= ((_.0 closure)) ((_.1 closure)) ((_.3 closure)))
     (num _.2) (sym _.0 _.1 _.3) (absento (closure _.4)))
    (((lambda (_.0) ((lambda (_.1) 6) (lambda (_.2) _.3))) _.4)
     (=/= ((_.0 closure)) ((_.1 closure)) ((_.2 closure)))
     (num _.4) (sym _.0 _.1 _.2) (absento (closure _.3)))
    (((lambda (_.0) ((lambda (_.1) _.0) _.2)) 6)
     (=/= ((_.0 _.1)) ((_.0 closure)) ((_.1 closure)))
     (num _.2) (sym _.0 _.1))
    (((lambda (lambda) (lambda _.0)) (lambda (_.1) 6))
     (=/= ((_.1 closure))) (num _.0) (sym _.1))))

(test "Scheme-interpreter-6"
  (run* (q)
    (eval-expo
     `(letrec ((eval-expr
                (lambda (expr env)
                  (match expr
                    [(? number? n) n]
                    [(? symbol? x) (env x)]
                    [`(lambda (,(? symbol? x)) ,body)
                     (lambda (a)
                       (eval-expr body (lambda (y)
                                         (if (equal? x y)
                                             a
                                             (env y)))))]
                    [`(,rator ,rand)
                     ((eval-expr rator env) (eval-expr rand env))]))))
        (eval-expr '(lambda (z) z) (lambda (y) ((lambda (z) z)))))
     '()
     q))
  '((closure
     (lambda (a)
       (eval-expr body
                  (lambda (y) (if (equal? x y) a (env y)))))
     (ext-env body z
              (ext-env x z
                       (ext-env env
                                (closure (lambda (y) ((lambda (z) z)))
                                         (ext-rec
                                          ((eval-expr
                                            (lambda (expr env)
                                              (match expr ((? number? n) n)
                                                     ((? symbol? x) (env x))
                                                     (`(lambda (,(? symbol? x)) ,body)
                                                      (lambda (a)
                                                        (eval-expr body
                                                                   (lambda (y)
                                                                     (if (equal? x y) a (env y))))))
                                                     (`(,rator ,rand)
                                                      ((eval-expr rator env)
                                                       (eval-expr rand env)))))))
                                          ()))
                                (ext-env expr (lambda (z) z)
                                         (ext-rec
                                          ((eval-expr
                                            (lambda (expr env)
                                              (match expr ((? number? n) n)
                                                     ((? symbol? x) (env x))
                                                     (`(lambda (,(? symbol? x)) ,body)
                                                      (lambda (a)
                                                        (eval-expr body
                                                                   (lambda (y)
                                                                     (if (equal? x y) a (env y))))))
                                                     (`(,rator ,rand)
                                                      ((eval-expr rator env)
                                                       (eval-expr rand env)))))))
                                          ()))))))))

(test "Scheme-interpreter-7"
  (run 3 (q)
    (eval-expo
     `(letrec ((eval-expr
                (lambda (expr env)
                  (match expr
                    [(? number? n) n]
                    [(? symbol? x) (env x)]
                    [`(lambda (,(? symbol? x)) ,body)
                     (lambda (a)
                       (eval-expr body (lambda (y)
                                         (if (equal? x y)
                                             a
                                             (env y)))))]
                    [`(,rator ,rand)
                     ((eval-expr rator env) (eval-expr rand env))]))))
        (eval-expr ',q (lambda (y) ((lambda (z) z)))))
     '()
     '(closure
       (lambda (a)
         (eval-expr body
                    (lambda (y) (if (equal? x y) a (env y)))))
       (ext-env body z
                (ext-env x z
                         (ext-env env
                                  (closure (lambda (y) ((lambda (z) z)))
                                           (ext-rec
                                            ((eval-expr
                                              (lambda (expr env)
                                                (match expr ((? number? n) n)
                                                       ((? symbol? x) (env x))
                                                       (`(lambda (,(? symbol? x)) ,body)
                                                        (lambda (a)
                                                          (eval-expr body
                                                                     (lambda (y)
                                                                       (if (equal? x y) a (env y))))))
                                                       (`(,rator ,rand)
                                                        ((eval-expr rator env)
                                                         (eval-expr rand env)))))))
                                            ()))
                                  (ext-env expr (lambda (z) z)
                                           (ext-rec
                                            ((eval-expr
                                              (lambda (expr env)
                                                (match expr ((? number? n) n)
                                                       ((? symbol? x) (env x))
                                                       (`(lambda (,(? symbol? x)) ,body)
                                                        (lambda (a)
                                                          (eval-expr body
                                                                     (lambda (y)
                                                                       (if (equal? x y) a (env y))))))
                                                       (`(,rator ,rand)
                                                        ((eval-expr rator env)
                                                         (eval-expr rand env)))))))
                                            ()))))))))
  '((lambda (z) z)
    (((lambda (_.0) _.0) (lambda (z) z))
     (=/= ((_.0 closure)))
     (sym _.0))
    (((lambda (_.0) ((lambda (_.1) _.0) _.2)) (lambda (z) z))
     (=/= ((_.0 _.1)) ((_.0 closure)) ((_.1 closure)))
     (num _.2)
     (sym _.0 _.1))))

(test "Scheme-interpreter-8"
  (run* (q)
    (eval-expo
      `(letrec ((eval-expr
                 (lambda (expr env)
                   (match expr
                     [(? number? n) n]
                     [(? symbol? x) (env x)]
                     [`(lambda (,(? symbol? x)) ,body)
                      (lambda (a)
                        (eval-expr body (lambda (y)
                                          (if (equal? x y)
                                              a
                                              (env y)))))]
                     [`(,rator ,rand)
                      ((eval-expr rator env) (eval-expr rand env))]))))
         (eval-expr '(lambda (z) 6) (lambda (y) ((lambda (z) z)))))
      '()
      q))
  '((closure
     (lambda (a)
       (eval-expr body
                  (lambda (y) (if (equal? x y) a (env y)))))
     (ext-env body 6
              (ext-env x z
                       (ext-env env
                                (closure (lambda (y) ((lambda (z) z)))
                                         (ext-rec
                                          ((eval-expr
                                            (lambda (expr env)
                                              (match expr ((? number? n) n)
                                                     ((? symbol? x) (env x))
                                                     (`(lambda (,(? symbol? x)) ,body)
                                                      (lambda (a)
                                                        (eval-expr body
                                                                   (lambda (y)
                                                                     (if (equal? x y) a (env y))))))
                                                     (`(,rator ,rand)
                                                      ((eval-expr rator env)
                                                       (eval-expr rand env)))))))
                                          ()))
                                (ext-env expr (lambda (z) 6)
                                         (ext-rec
                                          ((eval-expr
                                            (lambda (expr env)
                                              (match expr ((? number? n) n)
                                                     ((? symbol? x) (env x))
                                                     (`(lambda (,(? symbol? x)) ,body)
                                                      (lambda (a)
                                                        (eval-expr body
                                                                   (lambda (y)
                                                                     (if (equal? x y) a (env y))))))
                                                     (`(,rator ,rand)
                                                      ((eval-expr rator env)
                                                       (eval-expr rand env)))))))
                                          ()))))))))







#!eof

`(letrec ((eval-expr
           (lambda (expr env)
             (match expr
               [(? symbol? x) (env x)]
               [`(lambda (,(? symbol? x)) ,body)
                (lambda (a)
                  (eval-expr body (lambda (y)
                                    (if (equal? x y)
                                        a
                                        (env y)))))]
               [`(,rator ,rand)
                ((eval-expr rator env) (eval-expr rand env))]))))
   (eval-expr '((lambda (y) w) (lambda (z) z)) (lambda (y) ((lambda (z) z)))))



;; match tests
(test "match-0"
  (run* (q) (eval-expo '(match 5) '() q))
  '())

(test "match-1a"
  (run* (q) (eval-expo '(match 5 [5 6]) '() q))
  '(6))

(test "match-1b"
  (run* (q) (eval-expo '(match 5 [x 6]) '() q))
  '(6))

(test "match-1c"
  (run* (q) (eval-expo '(match 5 [x x]) '() q))
  '(5))

(test "match-1d"
  (run* (q) (eval-expo '(match 5 [5 6] [7 8]) '() q))
  '(6))

(test "match-1e"
  (run* (q) (eval-expo '(match 5 [x 6] [y 7]) '() q))
  '(6))

(test "match-1f"
  (run* (q) (eval-expo '(match 5 [x 6] [x 7]) '() q))
  '(6))



(test "match-2"
  (run* (q) (eval-expo '(match (cons 5 6) [`(,x . ,y) 7]) '() q))
  '(7))

(test "match-3"
  (run* (q) (eval-expo '(match (cons 5 6) [`(,x . ,y) x]) '() q))
  '(5))

(test "match-4"
  (run* (q) (eval-expo '(match (cons 5 6) [`(,x . ,y) y]) '() q))
  '(6))

(test "match-5"
  (run* (q) (eval-expo '(match (cons 5 6) [7 8]) '() q))
  '())

(test "match-6"
  (run* (q) (eval-expo '(match 4 [7 8]) '() q))
  '())

(test "match-7"
  (run* (q) (eval-expo '(match '(lambda (y) (y z)) [`(lambda (,x) ,body) (cons x body)]) '() q))
  '((y y z)))

(test "match-8"
  (run* (q) (eval-expo '(match '((lambda (y) (y z)) 5) [`(,rator ,rand) (cons rator (cons rand '()))]) '() q))
  '(((lambda (y) (y z)) 5)))

(test "match-9"
  (run* (q) (eval-expo
              '(match '((lambda (y) (y z)) 5)
                 [`(lambda (,x) ,body) (cons 'lambda-expr (cons x (cons body '())))]
                 [`(,rator ,rand) (cons 'app-expr (cons rator (cons rand '())))])
              '()
              q))
  '((app-expr (lambda (y) (y z)) 5)))

(test "match-10"
  (run* (q) (eval-expo
              '(match '(lambda (y) (y z))
                 [`(lambda (,x) ,body) (cons 'lambda-expr (cons x (cons body '())))]
                 [`(,rator ,rand) (cons 'app-expr (cons rator (cons rand '())))])
              '()
              q))
  '((lambda-expr y (y z))))

(test "match-11"
  (run* (q) (eval-expo
              '(match '(5 6 7)
                 [`(,x ,y ,z) (cons 'first (cons x (cons y (cons z '()))))]
                 [`(,u ,v ,w) (cons 'second (cons u (cons v (cons w '()))))])
              '()
              q))
  '((first 5 6 7)))

(test "match-12"
  (run* (q) (eval-expo
              '(match '(5 6 7)
                 [`(,x ,y ,x) (cons 'first (cons x (cons y (cons x '()))))]
                 [`(,u ,v ,w) (cons 'second (cons u (cons v (cons w '()))))])
              '()
              q))
  '((second 5 6 7)))

(test "match-13"
  (run* (q) (eval-expo
              '(match '(5 6 7)
                 [`(,x ,y ,x) (cons 'first (cons x (cons y (cons x '()))))]
                 [`(,x ,y ,z) (cons 'second (cons x (cons y (cons z '()))))])
              '()
              q))
  '((second 5 6 7)))

(test "match-14"
  (run* (q) (eval-expo
              '(match '(5 6 5)
                 [`(,x ,y ,z) (cons 'first (cons x (cons y (cons z '()))))]
                 [`(,u ,v ,w) (cons 'second (cons u (cons v (cons w '()))))])
              '()
              q))
  '((first 5 6 5)))

(test "match-15"
  (run* (q) (eval-expo
              '(match '(5 6 5)
                 [`(,x ,y ,x) (cons 'first (cons x (cons y (cons x '()))))]
                 [`(,u ,v ,w) (cons 'second (cons u (cons v (cons w '()))))])
              '()
              q))
  '((first 5 6 5)))

(test "match-16"
  (run* (q) (eval-expo
              '(match '(5 6 5)
                 [`(,x ,y ,x) (cons 'first (cons x (cons y (cons x '()))))]
                 [`(,x ,y ,z) (cons 'second (cons x (cons y (cons z '()))))])
              '()
              q))
  '((first 5 6 5)))


(test "match-17"
  (run* (q) (eval-expo '(match '#t [#f 6] [#t 8]) '() q))
  '(8))



;; Racket-compatible 'symbol?' predicate syntax
;;
;; `(lambda (,(? symbol? x)) ,body)
;;
(test "match-symbol-0a"
  (run* (q) (eval-expo
             '(match 'w
                [(? symbol? y) y])
             '()
             q))
  '(w))

(test "match-symbol-1"
  (run* (q) (eval-expo
             '(match '(lambda (y) (y z))
                [`(lambda (,(? symbol? x)) ,body) (cons x body)])
             '()
             q))
  '((y y z)))

(test "match-symbol-2"
  (run 1 (pat out) (eval-expo `(match ,pat [`(lambda (,(? symbol? x)) ,body) (cons x body)]) '() out))
  '((('(lambda (_.0) _.1)
      (_.0 . _.1))
     (=/= ((_.0 closure)))
   (sym _.0)
   (absento (closure _.1)))))

(test "match-symbol-3"
  (run 3 (against out) (eval-expo `(match ,against [`(lambda (,(? symbol? x)) ,body) (cons x body)]) '() out))
  '((('(lambda (_.0) _.1) (_.0 . _.1))
     (=/= ((_.0 closure)))
     (sym _.0)
     (absento (closure _.1)))
    (((match _.0 (_.0 '(lambda (_.1) _.2)) . _.3) (_.1 . _.2))
     (=/= ((_.1 closure)))
     (num _.0)
     (sym _.1)
     (absento (closure _.2)))
    (((list 'lambda '(_.0) _.1) (_.0 . _.1))
     (=/= ((_.0 closure)))
     (num _.1)
     (sym _.0))))

(test "match-symbol-4"
  (run 3 (body)
    (eval-expo
      `(match '(lambda (y) (y z))
         [`(lambda (,(? symbol? x)) ,body) ,body])
      '()
      '(y y z)))
  '('(y y z)
    ((match _.0 (_.0 '(y y z)) . _.1)
     (num _.0))
    (list 'y 'y 'z)))




(test "match-1a-backwards"
  (run* (q) (eval-expo `(match 5
                          [,q 6])
                       '()
                       '6))
  '(5
    `5
    (_.0 (sym _.0))
    ((? number? _.0) (sym _.0))
    (`,_.0 (sym _.0))
    (`,(? number? _.0) (sym _.0))))


(test "match-1c-backwards"
  (run* (q) (eval-expo `(match 5 [,q x]) '() 5))
  '(x
    (? number? x)
    `,x
    `,(? number? x)))

(test "match-8-backwards-verify-a"
  (run* (q)
    (eval-expo
      '(match '((lambda (y) (y z)) 5) [`(,rator ,rand unquote _.0) (cons rator (cons rand '()))])
      '()
      q))
  '(((lambda (y) (y z)) 5)))

(test "match-8-backwards-verify-b"
  (run* (q)
    (eval-expo
      '(match '((lambda (y) (y z)) 5) [`(,rator ,rand unquote _.0) (cons rator (cons rand '()))])
      '()
      q))
  '(((lambda (y) (y z)) 5)))

(test "match-8-backwards-verify-c"
  (run* (q)
    (eval-expo
      '(match '((lambda (y) (y z)) 5) [`(,rator ,rand unquote foo) (cons rator (cons rand '()))])
      '()
      q))
  '(((lambda (y) (y z)) 5)))

(test "match-8-backwards-verify-d"
  (run* (q)
    (eval-expo
      '(match '((lambda (y) (y z)) 5) [`(,rator ,rand . (unquote foo)) (cons rator (cons rand '()))])
      '()
      q))
  '(((lambda (y) (y z)) 5)))

(test "match-8-backwards-verify-e"
  (run* (q)
    (eval-expo
      '(match '((lambda (y) (y z)) 5) [`(,rator ,rand . ,foo) (cons rator (cons rand '()))])
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
  '((closure (lambda (x) x) ())))

(test "eval-expo-4"
  (run* (q) (eval-expo '((lambda (x) x) 5) '() q))
  '(5))

(test "eval-expo-5"
  (run* (q) (eval-expo '((lambda (x) (lambda (y) x)) 5) '() q))
  '((closure (lambda (y) x) (ext-env x 5 ()))))



(test "quine-1"
  (run 6 (q) (eval-expo q '() q))
  '((_.0 (num _.0))
    #t
    #f
    (((lambda (_.0) (list _.0 (list 'quote _.0)))
      '(lambda (_.0) (list _.0 (list 'quote _.0))))
     (=/= ((_.0 closure)) ((_.0 list)) ((_.0 quote)))
     (sym _.0))
    (((lambda (_.0)
        (list (list 'lambda '(_.0) _.0) (list 'quote _.0)))
      '(list (list 'lambda '(_.0) _.0) (list 'quote _.0)))
     (=/= ((_.0 closure)) ((_.0 list)) ((_.0 quote)))
     (sym _.0))
    (((lambda (_.0)
        (list _.0 (list (match _.1 (_.1 'quote) . _.2) _.0)))
      '(lambda (_.0)
         (list _.0 (list (match _.1 (_.1 'quote) . _.2) _.0))))
     (=/= ((_.0 closure)) ((_.0 list)) ((_.0 match)))
     (num _.1)
     (sym _.0)
     (absento (closure _.2)))))

(test "closure-generation"
  (run 10 (q)
    (eval-expo
     q
     '()
     '(closure (lambda (x) x) ())))
  '((lambda (x) x)
    ((match _.0 (_.0 (lambda (x) x)) . _.1)
     (num _.0))
    ((lambda () (lambda (x) x)))
    ((match '_.0 (_.0 (lambda (x) x)) . _.1)
     (num _.0))
    ((match _.0 (`_.0 (lambda (x) x)) . _.1)
     (num _.0))
    ((match _.0 (_.1 _.2) (_.0 (lambda (x) x)) . _.3)
     (=/= ((_.0 _.1)))
     (num _.0 _.1))
    (match '#f (#f (lambda (x) x)) . _.0)
    (match '#t (#t (lambda (x) x)) . _.0)
    ((match '_.0 (_.1 _.2) (_.0 (lambda (x) x)) . _.3)
     (=/= ((_.0 _.1)))
     (num _.0 _.1))
    ((match _.0 (#f _.1) (_.0 (lambda (x) x)) . _.2)
     (num _.0))))


(printf "Long running tests...\n")

(printf "This test takes a while...\n")
(test "match-8-backwards"
  (run* (q)
    (eval-expo
      `(match '((lambda (y) (y z)) 5)
         [,q (cons rator (cons rand '()))])
      '()
      '((lambda (y) (y z)) 5)))
  '(`(,rator ,rand)
    `(,rator ,(? number? rand))
    (`(,rator ,rand unquote _.0)
     (=/= ((_.0 cons)) ((_.0 quote))
          ((_.0 rand)) ((_.0 rator)))
     (sym _.0))
    (`(,rator ,(? number? rand) unquote _.0)
     (=/= ((_.0 cons)) ((_.0 quote))
          ((_.0 rand)) ((_.0 rator)))
     (sym _.0))))

(printf "This test takes a while...\n")
(test "match-8-backwards-b"
  (run* (q)
    (eval-expo
     `(match '((lambda (y) (y z)) w)
        [,q (cons rator (cons rand '()))])
     '()
     '((lambda (y) (y z)) w)))
  '(`(,rator ,rand)
    `(,rator ,(? symbol? rand))
    (`(,rator ,rand unquote _.0)
     (=/= ((_.0 cons)) ((_.0 quote))
          ((_.0 rand)) ((_.0 rator)))
     (sym _.0))
    (`(,rator ,(? symbol? rand) unquote _.0)
     (=/= ((_.0 cons)) ((_.0 quote))
          ((_.0 rand)) ((_.0 rator)))
     (sym _.0))))












;; Standard Scheme definition of append.  I've wrapped the definition
;; in a 'let' to avoid shadowing Scheme's built-in 'append'
;; definition.
(let ()
  
  (define append
    (lambda (l s)
      (cond
        ((null? l) s)
        (else (cons (car l) (append (cdr l) s))))))

  (test "Scheme append-1"
    (append '(a b c) '(d e))
    '(a b c d e))
  
  )


;; Our normal relational 'appendo' definition, written in miniKanren.
;; 'appendo' doesn't look very Scheme-like, unfortunately.
(let ()

  (define appendo
    (lambda (l s out)
      (conde
        ((== '() l) (== s out))
        ((fresh (a d res)
           (== `(,a . ,d) l)
           (== `(,a . ,res) out)
           (appendo d s res))))))

  (test "appendo-1"
    (run* (q) (appendo '(a b c) '(d e) q))
    '((a b c d e)))

  (test "appendo-2"
    (run* (q) (appendo '(a b c) q '(a b c d e)))
    '((d e)))

  (test "appendo-3"
    (run* (x y) (appendo x y '(a b c d e)))
    '((() (a b c d e))
      ((a) (b c d e))
      ((a b) (c d e))
      ((a b c) (d e))
      ((a b c d) (e))
      ((a b c d e) ())))

  (test "appendo-4"
    (run 5 (x y z) (appendo x y z))
    '((() _.0 _.0)
      ((_.0) _.1 (_.0 . _.1))
      ((_.0 _.1) _.2 (_.0 _.1 . _.2))
      ((_.0 _.1 _.2) _.3 (_.0 _.1 _.2 . _.3))
      ((_.0 _.1 _.2 _.3) _.4 (_.0 _.1 _.2 _.3 . _.4))))
  
  )

;; Even the pattern-matching version of 'appendo' doesn't look that
;; much like the Scheme code.
(let ()

  (define appendo
    (lambda (l s out)
      (matche (l s out)
        ((() ,s ,s))
        (((,a . ,d) ,s (,a . ,res)) (appendo d s res)))))

  (test "appendo-1"
    (run* (q) (appendo '(a b c) '(d e) q))
    '((a b c d e)))

  (test "appendo-2"
    (run* (q) (appendo '(a b c) q '(a b c d e)))
    '((d e)))

  (test "appendo-3"
    (run* (x y) (appendo x y '(a b c d e)))
    '((() (a b c d e))
      ((a) (b c d e))
      ((a b) (c d e))
      ((a b c) (d e))
      ((a b c d) (e))
      ((a b c d e) ())))

  (test "appendo-4"
    (run 5 (x y z) (appendo x y z))
    '((() _.0 _.0)
      ((_.0) _.1 (_.0 . _.1))
      ((_.0 _.1) _.2 (_.0 _.1 . _.2))
      ((_.0 _.1 _.2) _.3 (_.0 _.1 _.2 . _.3))
      ((_.0 _.1 _.2 _.3) _.4 (_.0 _.1 _.2 _.3 . _.4))))
  
  )



;; With the relational Scheme interpreter written in miniKanren, we
;; can write the *Scheme* definition of 'append', and treat that
;; *function* as a *relation*.  This is because the interpreter itself
;; is a relation.
;;
;; Running append "forwards":
(test "Scheme-append-under-relational-interpreter-1"
  (run* (q)
    (evalo
     '(letrec ((append (lambda (l s)
                         (if (null? l)
                             s
                             (cons (car l) (append (cdr l) s))))))
        (append '(a b c) '(d e)))
     q))
  '((a b c d e)))

;; Running append "backwards:"
(test "Scheme-append-under-relational-interpreter-2"
  (run 6 (x y)
    (evalo
     `(letrec ((append (lambda (l s)
                         (if (null? l)
                             s
                             (cons (car l) (append (cdr l) s))))))
        (append ,x ,y))
     '(a b c d e)))
  '(('() '(a b c d e))
    ('(a) '(b c d e))
    ('(a b) '(c d e))
    ('(a b c) '(d e))
    ('(a b c d) '(e))
    ('(a b c d e) '())))

;; Replacing 'run 6' with 'run*' in
;; Scheme-append-under-relational-interpreter-2 results in divergence
;; (looping forever).  This seems bad.  Aren't there only 6 answers?

;; Let's try to generate a seventh answer:
(test "Scheme-append-under-relational-interpreter-3"
  (run 7 (x y)
    (evalo
     `(letrec ((append (lambda (l s)
                         (if (null? l)
                             s
                             (cons (car l) (append (cdr l) s))))))
        (append ,x ,y))
     '(a b c d e)))
  '(('() '(a b c d e))
    ('(a) '(b c d e))
    ('(a b) '(c d e))
    ('(a b c) '(d e))
    ('(a b c d) '(e))
    ('(a b c d e) '())
    ('(a b c d e) (list))))

;; Whoa!  The last answer has a call to 'list' with no arguments,
;; producing the empty list!  Because we are running 'append' in the
;; context of the relational Scheme interpreter, the logic variables
;; 'x' and 'y' in the body of the 'letrec' represent *arbitrary Scheme
;; expressions* that evaluate to lists of symbols.

;; Let's look at a few more answers:
(test "Scheme-append-under-relational-interpreter-4"
  (run 20 (x y)
    (evalo
     `(letrec ((append (lambda (l s)
                         (if (null? l)
                             s
                             (cons (car l) (append (cdr l) s))))))
        (append ,x ,y))
     '(a b c d e)))
  '(('() '(a b c d e))
    ('(a) '(b c d e))
    ('(a b) '(c d e))
    ('(a b c) '(d e))
    ('(a b c d) '(e))
    ('(a b c d e) '())
    ('(a b c d e) (list))
    ('(a b c d) (list 'e))
    ((list) '(a b c d e))
    (('() (match _.0 (_.0 '(a b c d e)) . _.1)) (num _.0))
    (('(a) (match _.0 (_.0 '(b c d e)) . _.1)) (num _.0))
    (('(a b) (match _.0 (_.0 '(c d e)) . _.1)) (num _.0))
    (('(a b c) (match _.0 (_.0 '(d e)) . _.1)) (num _.0))
    (('(a b c d) (match _.0 (_.0 '(e)) . _.1)) (num _.0))
    (('(a b c d e) (match _.0 (_.0 '()) . _.1)) (num _.0))
    ('(a b c) (list 'd 'e))
    (('() (match '_.0 (_.0 '(a b c d e)) . _.1)) (num _.0))
    (('(a) (match '_.0 (_.0 '(b c d e)) . _.1)) (num _.0))
    (('(a b) (match '_.0 (_.0 '(c d e)) . _.1)) (num _.0))
    (('(a b c) (match '_.0 (_.0 '(d e)) . _.1)) (num _.0))))

;; Sure enough, later answers call 'list', and even use variadic
;; 'lambda' and procedure application.  So our Scheme 'append',
;; running in the relational interpreter, is more general than
;; 'appendo'!

;; We can recapture the behavior of 'appendo', in which we restrict
;; the arguments to lists of values (rather than expressions that
;; *evaluate* to lists of values) by a careful use of 'quote' inside
;; the body of the 'letrec':
(test "Scheme-append-under-relational-interpreter-5"
  (run* (x y)
    (evalo
     `(letrec ((append (lambda (l s)
                         (if (null? l)
                             s
                             (cons (car l) (append (cdr l) s))))))
        (append (quote ,x) (quote ,y)))
     '(a b c d e)))
  '((() (a b c d e))
    ((a) (b c d e))
    ((a b) (c d e))
    ((a b c) (d e))
    ((a b c d) (e))
    ((a b c d e) ())))


;; In addition to inferring the two list arguments in an 'append'
;; call, we can infer the actual use of 'append' in the call!

;; Our first attempt to infer the use of 'append' is unsuccessful.
;; miniKanren "cheats" by generating a variadic lambda expression
;; whose body returns the "output" list.
(test "infer-append-use-1"
  (run 1 (q)
    (evalo
     `(letrec ((append (lambda (l s)
                         (if (null? l)
                             s
                             (cons (car l) (append (cdr l) s))))))
        (,q '(a b c) '(d e)))
     '(a b c d e)))
  '(((lambda _.0 '(a b c d e)) (=/= ((_.0 quote))) (sym _.0))))

;; We can use the 'absento' constraint to keep miniKanren from
;; cheating.  The constraint '(absento 'a q)' ensures that the symbol
;; 'a'---which occurs in both the input to the call and the output---
;; does not occur in the expression we are trying to infer.
;;
;; This results in the expected answer, 'append', and a second
;; expression that also evaluates to the append procedure.
(test "infer-append-use-2"
  (run 2 (q)
    (evalo
     `(letrec ((append (lambda (l s)
                         (if (null? l)
                             s
                             (cons (car l) (append (cdr l) s))))))
        (,q '(a b c) '(d e)))
     '(a b c d e))
    (absento 'a q))
  '(append
    (((lambda _.0 append))
     (=/= ((_.0 a)) ((_.0 append)))
     (sym _.0))))


;; We can also infer missing sub-expressions from the definition of
;; 'append'.  Here we infer the missing '(car l)' call from the 'else'
;; branch of the 'if' expression.  The second answer is an expression
;; whose behavior is equivalent to that of '(car l)'.
;;
;; Several subexpressions are quick to infer.  Other subexpressions
;; take a very long time to infer.  The interpreter cannot (yet) be
;; practically used for example-based program synthesis of programs
;; like 'append', but there may be improvements to the miniKanren
;; implementation, the relational interpreter, and our inference
;; techniques which would make example-based synthesis feasible in
;; practice.  We are currently exploring this research area.
(test "infer-car-1"
  (run 2 (q)
    (absento 'a q)
    (evalo
     `(letrec ((append (lambda (l s)
                         (if (null? l)
                             s
                             (cons ,q (append (cdr l) s))))))
        (append '(a b c) '(d e)))
     '(a b c d e)))
  '((car l)
    ((match l (`(,_.0 unquote _.1) _.0) . _.2)
     (=/= ((_.0 _.1)) ((_.0 a)) ((_.1 a)))
     (sym _.0 _.1)
     (absento (a _.2)))))


;; One fun thing we can do with the relational interpreter is generate
;; Scheme programs that evaluate to a given value.  For example, here
;; are ten Scheme expressions that evaluate to the list '(I love you)'.
(test "I-love-you-1"
  (run 10 (q)
    (evalo
     q
     '(I love you)))
  '('(I love you)
    (list 'I 'love 'you)
    ((match _.0 (_.0 '(I love you)) . _.1)
     (num _.0))
    (((lambda _.0 '(I love you)))
     (=/= ((_.0 quote)))
     (sym _.0))
    (((lambda _.0 '(I love you)) _.1)
     (=/= ((_.0 quote)))
     (num _.1) (sym _.0))
    (((lambda _.0 '(I love you)) '_.1)
     (=/= ((_.0 quote)))
     (sym _.0)
     (absento (closure _.1)))
    ((match '_.0 (_.0 '(I love you)) . _.1)
     (num _.0))
    (((lambda _.0 '(I love you)) _.1 _.2)
     (=/= ((_.0 quote)))
     (num _.1 _.2)
     (sym _.0))
    (((lambda _.0 '(I love you)) _.1 '_.2)
     (=/= ((_.0 quote)))
     (num _.1)
     (sym _.0)
     (absento (closure _.2)))
    ((lambda () '(I love you)))))


(test "I-love-you-append"
  (run 10 (q)
    (evalo
     `(letrec ((append (lambda (l s)
                         (if (null? l)
                             s
                             (cons (car l) (append (cdr l) s))))))
        ,q)
     '(I love you)))
  '('(I love you)
    ((match _.0 (_.0 '(I love you)) . _.1)
     (num _.0))
    (list 'I 'love 'you)
    ((match '_.0 (_.0 '(I love you)) . _.1)
     (num _.0))
    (((lambda _.0 '(I love you)))
     (=/= ((_.0 quote)))
     (sym _.0))
    ((match _.0 (`_.0 '(I love you)) . _.1)
     (num _.0))
    (((lambda _.0 '(I love you)) _.1)
     (=/= ((_.0 quote)))
     (num _.1)
     (sym _.0))
    ((match _.0 (_.1 _.2) (_.0 '(I love you)) . _.3)
     (=/= ((_.0 _.1)))
     (num _.0 _.1))
    (match '#f (#f '(I love you)) . _.0)
    ((list (match _.0 (_.0 'I) . _.1) 'love 'you)
     (num _.0))))


;; Our relational interpreter can also generate quines, which are
;; Scheme expressions that evaluate to themselves.
(test "simple quines"
  (run 5 (q) (evalo q q))
  '((_.0 (num _.0))
    #t
    #f
    (((lambda (_.0) (list _.0 (list 'quote _.0)))
      '(lambda (_.0) (list _.0 (list 'quote _.0))))
     (=/= ((_.0 closure)) ((_.0 list)) ((_.0 quote)))
     (sym _.0))
    (((lambda (_.0)
        (list (list 'lambda '(_.0) _.0) (list 'quote _.0)))
      '(list (list 'lambda '(_.0) _.0) (list 'quote _.0)))
     (=/= ((_.0 closure)) ((_.0 list)) ((_.0 quote)))
     (sym _.0))))

(test "quines-in-context-of-append"
  (run 10 (q)
    (evalo
     `(letrec ((append (lambda (l s)
                         (if (null? l)
                             s
                             (cons (car l) (append (cdr l) s))))))
        ,q)
     q))
  '((_.0 (num _.0))
    #t
    #f
    (((lambda (_.0) (list _.0 (list 'quote _.0)))
      '(lambda (_.0) (list _.0 (list 'quote _.0))))
     (=/= ((_.0 closure)) ((_.0 list)) ((_.0 quote)))
     (sym _.0))
    (((lambda (_.0)
        (list _.0 (list (match _.1 (_.1 'quote) . _.2) _.0)))
      '(lambda (_.0)
         (list _.0 (list (match _.1 (_.1 'quote) . _.2) _.0))))
     (=/= ((_.0 closure)) ((_.0 list)) ((_.0 match)))
     (num _.1) (sym _.0) (absento (closure _.2)))
    (((lambda (_.0)
        (list (list 'lambda '(_.0) _.0) (list 'quote _.0)))
      '(list (list 'lambda '(_.0) _.0) (list 'quote _.0)))
     (=/= ((_.0 closure)) ((_.0 list)) ((_.0 quote)))
     (sym _.0))
    (((lambda (_.0) (list _.0 ((lambda _.1 _.1) 'quote _.0)))
      '(lambda (_.0) (list _.0 ((lambda _.1 _.1) 'quote _.0))))
     (=/= ((_.0 closure)) ((_.0 lambda)) ((_.0 list))
          ((_.0 quote)) ((_.1 closure)))
     (sym _.0 _.1))
    (((lambda (_.0) ((lambda _.1 _.1) _.0 (list 'quote _.0)))
      '(lambda (_.0) ((lambda _.1 _.1) _.0 (list 'quote _.0))))
     (=/= ((_.0 closure)) ((_.0 lambda)) ((_.0 list))
          ((_.0 quote)) ((_.1 closure)))
     (sym _.0 _.1))
    (((lambda (_.0)
        (list ((lambda _.1 _.0)) (list 'quote _.0)))
      '(lambda (_.0)
         (list ((lambda _.1 _.0)) (list 'quote _.0))))
     (=/= ((_.0 _.1)) ((_.0 closure)) ((_.0 lambda))
          ((_.0 list)) ((_.0 quote)) ((_.1 closure)))
     (sym _.0 _.1))
    (((lambda (_.0)
        (list (list 'lambda '(_.0) _.0)
              (list (match _.1 (_.1 'quote) . _.2) _.0)))
      '(list (list 'lambda '(_.0) _.0)
             (list (match _.1 (_.1 'quote) . _.2) _.0)))
     (=/= ((_.0 closure)) ((_.0 list)) ((_.0 match))
          ((_.0 quote)))
     (num _.1) (sym _.0) (absento (closure _.2)))))
