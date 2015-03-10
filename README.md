# relational-interp-with-match
Relational Scheme interpreter, written in miniKanren, with a pattern-matcher supporting a subset of Racket's `match` syntax.

Joint work with Michael Ballantyne


Grammar for `match`:

```
(match ,expr ,clause ,clauses ...)

clause ::= (,toppattern ,expr)

toppattern ::= selfevalliteral | pattern | (quasiquote ,quasipattern)

pattern ::= var | (? ,pred ,var)

quasipattern ::= literal | (,quasipattern . ,quasipattern) | (unquote ,pattern)

selfevalliteral ::= number | #t | #f

literal ::= selfevalliteral | symbol | ()

var ::= <symbol>

pred ::= symbol? | number?
```


TODO:

* Add support for `letrec` and multiple argument `lambda` and application.  `letrec` support will probably require moving to a tagged environment.
* Try adding another argument to `match-pattern` so we can accumulate the environment rather than calling `appendo` afterwards.  This may give better performance, and is probably necessary for handling tagged environments, as in `letrec`.
* Try writing a simple theorem prover, etc., using `match`.
* Figure out whether it is necessary to add `(absento 'closure pattern)` or the equivalent.  I don't think so, but I'm not certain.
* Figure out why `run*` for test `match-8-backwards` takes so long, even though run 2 is fast.  Is there a way to speed this up?
