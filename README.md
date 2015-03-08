# relational-interp-with-match
Relational Scheme interpreter, written in miniKanren, with Racket-style pattern matcher

Joint work with Michael Ballantyne

TODO:

* Add  `number?` inline predicate to the pattern language.
* Specify the grammar for the pattern language and matcher.
* Add support for `letrec` and multiple argument `lambda` and application.  `letrec` support will probably require moving to a tagged environment.
* Try adding another argument to `match-pattern` so we can accumulate the enviornment rather than calling `appendo` afterwards.  This may give better performance, and is probably necessary for handling tagged environments, as in `letrec`.
* Write a simple higher-order match-based Scheme interpreter using `match`.  Also try writing a simple theorem prover, etc.
* Figure out whether it is necessary to add `(absento 'closure pattern)` or the equivalent.  I don't think so, but I'm not certain.
* Figure out why `run*` for test `match-8-backwards` takes so long, even though run 2 is fast.  Is there a way to speed this up?
