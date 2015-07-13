functor AbtUnify(Abt : ABT_UTIL) :>
        UNIFY
          where type t = Abt.t
          where type var = Abt.Variable.t =
struct
  open Abt
  type t = t
  type var = Variable.t

  exception Mismatch of t * t
  fun unify (l, r) = raise Fail ""
end
