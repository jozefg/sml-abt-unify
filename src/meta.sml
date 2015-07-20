functor MetaOperator(structure O : OPERATOR;
                     structure V : VARIABLE) :>
        META_OPERATOR
          where Variable = V
          where Operator = O =
struct
  structure Operator = O
  structure Variable = V

  datatype t = META of Variable.t | NORMAL of Operator.t

  fun eq (META v, META v') = Variable.eq (v, v')
    | eq (NORMAL p, NORMAL p') = Operator.eq (p, p')
    | eq (_, _) = false

  fun arity (META _) = #[]
    | arity (NORMAL p) = Operator.arity p

  fun toString (META v) = "@" ^ Variable.toString v
    | toString (NORMAL p) = Operator.toString p
end

functor MetaAbt(structure A : ABT) :>
        META_CONVERT where A = A =
struct
  structure A = A
  structure MetaOperator = MetaOperator(structure O = A.Operator
                                        structure V = A.Variable)

  structure WithO = Abt(structure Operator = MetaOperator
                        structure Variable = A.Variable)
  structure Meta = AbtUtil(WithO)

  open A
  infix $ \

  fun convert M =
    case out M of
        ` x => Meta.`` x
      | p $ es => Meta.$$ (MetaOperator.NORMAL p, Vector.map convert es)
      | v \ e => Meta.\\ (v, convert e)
end
