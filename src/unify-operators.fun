functor AbtUnifyOperators(structure O : META_OPERATOR
                          structure A : ABT_UTIL
                            where Operator = O) :>
                UNIFY
          where type t = Abt.t
          where type var = Abt.Variable.t =
struct

end
