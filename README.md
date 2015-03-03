# relational-interp-with-match
Relational Scheme interpreter, written in miniKanren, with Scheme pattern matcher

Joint work with Michael Ballantyne

TODO:

* Add Racket-style `symbol?` and `number?` inline predicates to the pattern language.
* Specify the grammar for the pattern language and matcher.
* Add support for `letrec` and multiple argument `lambda` and application.  `letrec` support will probably require moving to a tagged environment.
* Try adding another argument to `match-pattern` so we can accumulate the enviornment rather than calling `appendo` afterwards.  This may give better performance, and is probably necessary for handling tagged environments, as in `letrec`.
* Figure out why `run*` for test `match-8-backwards` takes so long (almost 4 seconds under Vicare), even though run 2 only takes ~4ms.  Is there a way to speed this up?