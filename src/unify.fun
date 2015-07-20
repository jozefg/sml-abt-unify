functor AbtUnify(Abt : ABT_UTIL) :>
        UNIFY
          where type t = Abt.t
          where type var = Abt.Variable.t =
struct
  open Abt
  infix $ \

  type t = t
  type var = Variable.t

  exception Mismatch of t * t

  (* This structure is the set of all the pairs of
   * variables we know to be equal when unifying terms.
   * For example, when unifying lam(x.x) with lam(y.y), we must
   * record the fact that x = y when going under the binder so that
   * we properly realize that these terms are alpha-equivalent.
   *)
  structure Pairs = SplaySet(structure Elem = struct
                               type t = Variable.t * Variable.t
                               fun eq ((l, r), (l', r')) =
                                 Variable.eq (l, l') andalso
                                 Variable.eq (r, r')

                               fun compare ((l, r), (l', r')) =
                                 case Variable.compare (l, l') of
                                     EQUAL => Variable.compare (r, r')
                                   | x => x
                              end)

  (* This sadly isn't in SplaySet already so we have to hack it
   * up. This is the equivalent of List.any though.
   *)
  fun anyPairs p pairs = Pairs.foldl (fn (x, b) => b orelse p x) false pairs


  (* Given a solution, apply it to a term by repeatedly substituting.
   * The conditions we have on solutions (no free vars in solution)
   *  make this well defined
   *)
  fun applySol sol e = List.foldl (fn ((v, e'), e) => subst e' v e) e sol

  (* We need a new notion of alpha equivalence which takes into
   * account this set of variables which we know to be equal. This
   * is otherwise the same as normal aequiv
   *)
  fun aequiv pairs (l, r) =
    case (out l, out r) of
        (` v, ` v') => Variable.eq (v, v') orelse Pairs.member pairs (v, v')
      | (x \ e, y \ e') => aequiv (Pairs.insert pairs (x, y)) (e, e')
      | (oper $ args, oper' $ args') =>
        Operator.eq (oper, oper')
        andalso Vector.all (aequiv pairs) (VectorPair.zip (args, args'))
      | _ => raise Mismatch (l, r)

  (* add sol pairs (v, e) will add (v, e) to the solution, if it
   * isn't already in there. If v is already in the solution this will
   * check to see what it was registered as and raise a Mismatch if
   * they aren't the same term after we apply the current solution to
   * e
   *)
  fun add sol pairs (v, e) =
    let
      val e = applySol sol e
      val sol = List.map (fn (v', e') => (v', subst e v e')) sol
    in
      case List.find (fn (v', _) => Variable.eq (v, v')) sol of
         NONE => (v, e) :: sol
       | SOME (_, e') =>
         if aequiv pairs (e, e')
         then sol
         else raise Mismatch (e, e')
    end

  (* This function checks to see whether a term contains any variables
   * we know to have been bound on the left. This prevents us from creating
   * a solution which contains *bound* variables in it. This makes no sense
   * because a bound variable doesn't make sense outside the context of its
   * binder.
   *)
  fun hasBoundVarsL pairs e =
    List.exists (fn v => anyPairs (fn (v', _) => Variable.eq (v, v')) pairs)
                (freeVariables e)

  fun hasBoundVarsR pairs e =
    List.exists (fn v => anyPairs (fn (_, v') => Variable.eq (v, v')) pairs)
                (freeVariables e)

  fun unify (l, r) =
    let
      fun go pairs sol (l, r) =
        case (out l, out r) of
            (* We want to avoid a bunch of (v, ` v)'s in the solution *)
            (` v, ` v') =>
            if Variable.eq (v, v') orelse Pairs.member pairs (v, v')
            then sol
            else add sol pairs (v, `` v')
          | (` v, _) =>
            if hasFree (r, v) orelse
               anyPairs (fn (v', _) => Variable.eq (v, v')) pairs orelse
               hasBoundVarsR pairs r
            then raise Mismatch (`` v, r)
            else add sol pairs (v, r)
          | (_, ` v) =>
            if hasFree (l, v) orelse
               anyPairs (fn (_, v') => Variable.eq (v, v')) pairs orelse
               hasBoundVarsL pairs l
            then raise Mismatch (`` v, l)
            else add sol pairs (v, l)
            (* This prevents us from failing to unifying aequiv terms *)
          | (x \ e, y \ e') => go (Pairs.insert pairs (x, y)) sol (e, e')
          | (oper $ args, oper' $ args') =>
            if Operator.eq (oper, oper')
            then Vector.foldr
                   (fn ((l, r), sol) => go pairs sol (l, r))
                   sol
                   (VectorPair.zip (args, args'))
            else raise Mismatch (l, r)
          | _ => raise Mismatch (l, r)
    in
      go Pairs.empty [] (l, r)
    end
end
