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

  fun add (v, e) sol =
    (v, List.foldl (fn ((v, e'), e) => subst e' v e) e sol) :: sol

  fun occursIn (v, e) = List.exists (fn v' => Variable.eq (v, v'))
                                    (freeVariables e)

  fun unify (l, r) =
    let
      fun go sol (l, r) =
        case (out l, out r) of
            (* We want to avoid a bunch of (v, ` v)'s in the solution *)
            (` v, ` v') =>
            if Variable.eq (v, v') then [] else add (v, `` v') sol
          | (` v, _) =>
            if occursIn (v, r)
            then raise Mismatch (`` v, r)
            else add (v, r) sol
          | (_, ` v) =>
            if occursIn (v, l)
            then raise Mismatch (`` v, l)
            else add (v, l) sol
            (* This prevents us from failing to unifying aequiv terms *)
          | (x \ e, y \ e') => go sol (e, subst (`` x) y e')
          | (oper $ args, oper' $ args') =>
            if Operator.eq (oper, oper')
            then Vector.foldr (fn ((l, r), sol) => go sol (l, r))
                              sol
                              (VectorPair.zip (args, args'))
            else raise Mismatch (l, r)
          | _ => raise Mismatch (l, r)
    in
      go [] (l, r)
    end
end
