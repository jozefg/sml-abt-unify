## sml-abt-unify

`sml-abt` defines a nice notion of terms with bound and free
variables. On top of this we can define a generic unification
algorithm for unifying to abt. This library provides a single functor

``` sml
    AbtUnify(Abt : ABT_UTIL)
      :> UNIFY
          where type t = Abt.t
          where type var = Abt.Variable.t
```

This `UNIFY` signature is just

``` sml
    signature UNIFY =
    sig
        (* Structures we're trying to merge *)
        type t

        (* The type of the free variables contained in t's *)
        type var

        (* This is thrown by unify when unification fails.
         * The left component is the subterm of the left argument we were
         * trying to unify. The right component is the subterm of the right
         * argument.
         *)
        exception Mismatch of t * t

        (* unify (l, r) produces a list of variables to terms with the
         * following conditions:
         *  1. Substituting each variable for the paired terms in l and r
         *     will yield a pair of alpha-equivalent terms
         *  2. If (v, e) is in the returned list than no free variables
         *     of e appear in the solution
         *  3. No variable appears twice in the solution
         *  4. All variables in the solution occur free in l or r
         *)
        val unify : t * t -> (var * t) list
    end
```

Open questions

 1. Lists suck, what's a better form for solutions (leave them
    abstract)?
 2. Is it fast enough yet?
