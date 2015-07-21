## sml-abt-unify

`sml-abt` defines a nice notion of terms with bound and free
variables. On top of this we can define a generic unification
algorithm for unifying to abt. The simplest usage of this functor is
with the `AbtUnify` functor.

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

        (* This behaves like [unify] but allows a unification term to
         * mention a bound variable. Because of this and limitations of
         * ABTs we cannot produce a substitution for it.
         *
         * The case where this differs is where we have a unification term
         * which appears under a binder and mentions a bound variable, eg
         * [lam(x.M)]. With [unify] this couldn't match [lam(x.x)] but it
         * does with [matches]. However, all occurences of [M] have to mention
         * the same bound variable, eg [ap(lam(x.M); lam(x.M))] doesn't match
         * [ap(lam(x.x); lam(x.x))] still since those [x]'s refer to different
         * bound variables.
         *)
        val matches : t * t -> bool
    end
```

In addition, we may not want to unify *all* free variables, just
certain blessed ones. For this, we have a unification algorithm for
unifying only "metavariables": special operators with an embedded
variable. The relevant signature here is in `meta.sig`.

``` sml
    signature META_OPERATOR =
    sig
        structure Operator : OPERATOR
        structure Variable : VARIABLE

        datatype t = META of Variable.t | NORMAL of Operator.t

        val eq : t * t -> bool
        val arity : t -> Arity.t
        val toString : t -> string
    end

    signature META_CONVERT =
    sig
        structure A : ABT

        structure MetaOperator : META_OPERATOR
          where Operator = A.Operator
          where Variable = A.Variable

        structure Meta : ABT
          where Operator = MetaOperator
          where Variable = A.Variable

        (* Converts and [A.t] into a [Meta.t] by turning each
         * operator into the corresponding [NORMAL] operator.
         * This otherwise leaves the rest of the structure intact.
         *)
        val convert : A.t -> Meta.t

        (* This converts an ABT to the corresponding meta-ABT
         * and converts all free variables to the appropriate [META]
         * operator. This means that any term created by [convertFree]
         * should be closed.
         *)
        val convertFree : A.t -> Meta.t

        (* Convert back into a normal [A.t] by exchanging each [META] for
         * a free variable.
         *)
        val unconvert : Meta.t -> A.t
    end
```

And we have a corresponding functor for unifying such ABTs called
`AbtUnifyOperators`.

``` sml
    functor AbtUnifyOperators(structure O : META_OPERATOR
                              structure A : ABT_UTIL
                                where Operator = O
                                where Variable = O.Variable) :>
            UNIFY
              where type t = A.t
              where type var = A.Variable.t =
```


There's a few tests in a the tests folder which demonstrate how to use
this library if the signature aren't helpful.

Open questions

 1. Lists suck, what's a better form for solutions (leave them
    abstract)?
 2. Is it fast enough yet?
