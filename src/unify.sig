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
     *)
    val unify : t * t -> (var * t) list
end
