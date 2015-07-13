## sml-abt-unify

`sml-abt` defines a nice notion of terms with bound and free
variables. On top of this we can define a generic unification
algorithm for unifying to abt. This library provides a single functor

``` sml
    AbtUnify(Abt : ABT_UTIL)
      : UNIFY
          where type t = Abt.t
          where type var = Abt.Variable.t
```

This `UNIFY` signature is just

``` sml
    signature UNIFY =
    sig
      type t
      type var
      val unify : t * t -> (var * t) list
    end
```

Open questions

 1. Lists suck, what's a better form for solutions (leave them
    abstract)?
 2. Is it fast enough yet?
