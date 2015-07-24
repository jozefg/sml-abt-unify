signature UNIFY =
sig
    (* Structures we're trying to merge *)
    type t

    (* The type of the free variables contained in t's *)
    type var

    structure Solution : DICT where type key = var
    type solution = t Solution.dict

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
    val unify : t * t -> solution

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
