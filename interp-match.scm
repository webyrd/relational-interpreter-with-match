(load "mk.scm")

;; match grammar
;;
;; (match ,expr ,clause ,clauses ...)
;;
;; clause ::= (,pattern ,expr)
;;
;; pattern ::= symbol | number | #t | #f | () | 

;; really should be a constraint built into miniKanren
(define not-symbolo
  (lambda (t)
    (conde
      [(== #f t)]
      [(== #t t)]
      [(numbero t)]
      [(fresh (a d)
         (== `(,a . ,d) t))])))

(define literalo
  (lambda (t)
    (conde
      [(numbero t)]
      [(symbolo t)]
      [(booleano t)]
      [(== '() t)])))

(define booleano
  (lambda (t)
    (conde
      [(== #f t)]
      [(== #t t)])))

(define (appendo l s out)
  (conde
    [(== '() l) (== s out)]
    [(fresh (a d res)
       (== `(,a . ,d) l)
       (== `(,a . ,res) out)
       (appendo d s res))]))

(define (lookupo x env val)
  (fresh (y v env^)
    (== `((,y . ,v) . ,env^) env)
    (conde
      [(== x y) (== v val)]
      [(=/= x y) (lookupo x env^ val)])))

(define (not-in-envo x env)
  (conde
    [(== '() env)]
    [(fresh (y v env^)
       (== `((,y . ,v) . ,env^) env)
       (=/= x y)
       (not-in-envo x env^))]))

(define (eval-expo expr env val)
  (conde
    [(numbero expr)
     (== expr val)]
    [(booleano expr)
     (== expr val)]
    [(== `(quote ,val) expr)
     (absento 'closure val)
     (not-in-envo 'quote env)]
    [(symbolo expr)
     (lookupo expr env val)]
    [(fresh (x body)
       (== `(lambda (,x) ,body) expr)
       (== `(closure ,x ,body ,env) val)
       (symbolo x)
       (not-in-envo 'lambda env))]
    [(fresh (e1 e2 v1 v2)
       (== `(cons ,e1 ,e2) expr)
       (== `(,v1 . ,v2) val)
       (not-in-envo 'cons env)
       (eval-expo e1 env v1)
       (eval-expo e2 env v2))]
    [(fresh (rator rand x body env^ arg)
       (== `(,rator ,rand) expr)
       (symbolo x)
       (eval-expo rator env `(closure ,x ,body ,env^))
       (eval-expo rand env arg)
       (eval-expo body `((,x . ,arg) . ,env^) val))]
    [(fresh (against-expr against-val clause clauses)
       (== `(match ,against-expr ,clause . ,clauses) expr)
       (not-in-envo 'match env)
       (eval-expo against-expr env against-val)
       (match-clauses against-val `(,clause . ,clauses) env val))]))

(define (match-clauses against-val clauses env val)
  (fresh (pattern result-expr d penv)
    (== `((,pattern ,result-expr) . ,d) clauses)
    (conde
      [(fresh (env^)
         (match-pattern pattern against-val '() penv)
         (appendo penv env env^)
         (eval-expo result-expr penv val))]
      [(not-match-pattern pattern against-val '() penv)
       (match-clauses against-val d env val)])))

(define (match-pattern pattern against-val penv penv-out)
  (conde
    [(== pattern against-val)
     (== penv penv-out)
     (literalo pattern)]   
    [(fresh (var val)
      (== (list 'unquote var) pattern)
      (symbolo var)
      (conde
        [(== against-val val)
         (== penv penv-out)
         (lookupo var penv val)]
        [(== `((,var . ,against-val) . ,penv) penv-out)
         (not-in-envo var penv)]))]
    [(fresh (var val)
      (== (list 'unquote `(? symbol? ,var)) pattern)
      (symbolo var)
      (symbolo against-val)
      (conde
        [(== against-val val)
         (== penv penv-out)
         (lookupo var penv val)]
        [(== `((,var . ,against-val) . ,penv) penv-out)
         (not-in-envo var penv)]))]    
    [(fresh (a d v1 v2 penv^)
       (== `(,a . ,d) pattern)
       (== `(,v1 . ,v2) against-val)
       (=/= 'unquote a)
       (match-pattern a v1 penv penv^)
       (match-pattern d v2 penv^ penv-out))]))

(define (not-match-pattern pattern against-val penv penv-out)
  (conde
    [(=/= pattern against-val)
     (== penv penv-out)
     (literalo pattern)]
    [(fresh (var val)
      (== (list 'unquote var) pattern)
      (=/= against-val val)
      (== penv penv-out)
      (symbolo var)
      (lookupo var penv val))]
    [(fresh (var val)
       (== (list 'unquote `(? symbol? ,var)) pattern)
       (== penv penv-out)
       (symbolo var)       
       (conde
         [(not-symbolo against-val)]
         [(=/= against-val val)          
          (symbolo against-val)
          (lookupo var penv val)]))]
    [(fresh (a d v1 v2 penv^)
       (== `(,a . ,d) pattern)
       (== `(,v1 . ,v2) against-val)
       (=/= 'unquote a)
       (conde
         [(not-match-pattern a v1 penv penv^)]
         [(match-pattern a v1 penv penv^)
          (not-match-pattern d v2 penv^ penv-out)]))]))
