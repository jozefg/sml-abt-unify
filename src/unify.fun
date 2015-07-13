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

  fun anyPairs p pairs = Pairs.foldl (fn (x, b) => b orelse p x) false pairs

  fun unify (l, r) =
    let
      fun go pairs sol (l, r) =
        case (out l, out r) of
            (* We want to avoid a bunch of (v, ` v)'s in the solution *)
            (` v, ` v') =>
            if Pairs.member pairs (v, v') then sol else add (v, `` v') sol
          | (` v, _) =>
            if occursIn (v, r) orelse
               anyPairs (fn (v', _) => Variable.eq (v, v')) pairs
            then raise Mismatch (`` v, r)
            else add (v, r) sol
          | (_, ` v) =>
            if occursIn (v, l) orelse
               anyPairs (fn (_, v') => Variable.eq (v, v')) pairs
            then raise Mismatch (`` v, l)
            else add (v, l) sol
            (* This prevents us from failing to unifying aequiv terms *)
          | (x \ e, y \ e') => go (Pairs.insert pairs (x, y)) sol (e, e')
          | (oper $ args, oper' $ args') =>
            if Operator.eq (oper, oper')
            then Vector.foldr (fn ((l, r), sol) => go pairs sol (l, r))
                              sol
                              (VectorPair.zip (args, args'))
            else raise Mismatch (l, r)
          | _ => raise Mismatch (l, r)
    in
      go Pairs.empty [] (l, r)
    end
end
