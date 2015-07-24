functor MetaOperator(structure O : OPERATOR;
                     structure V : VARIABLE) :>
        META_OPERATOR
          where Variable = V
          where Operator = O =
struct
  structure Operator = O
  structure Variable = V

  datatype t = META of Variable.t | WILD | NORMAL of Operator.t

  fun eq (META v, META v') = Variable.eq (v, v')
    | eq (NORMAL p, NORMAL p') = Operator.eq (p, p')
    | eq (WILD, WILD) = true
    | eq (_, _) = false

  fun arity (META _) = #[]
    | arity WILD = #[]
    | arity (NORMAL p) = Operator.arity p

  fun toString (META v) = "@" ^ Variable.toString v
    | toString WILD = "!"
    | toString (NORMAL p) = Operator.toString p
end

functor MetaAbt(A : ABT) :>
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

  structure BoundSet = SplaySet(structure Elem = Variable)

  fun convertFree M =
    let
      fun go bound M =
          case out M of
              ` x => if BoundSet.member bound x
                     then Meta.`` x
                     else Meta.$$ (MetaOperator.META x, #[])
            | p $ es => Meta.$$ (MetaOperator.NORMAL p,
                                 Vector.map (go bound) es)
            | v \ e => Meta.\\ (v, go (BoundSet.insert bound v) e)
    in
      go BoundSet.empty M
    end

  fun unconvert t M =
    case Meta.out M of
        Meta.` x => into (` x)
      | Meta.\ (x, e) => into (x \ unconvert t e)
      | Meta.$ (MetaOperator.META v, _) => into (` v)
      | Meta.$ (MetaOperator.WILD, _) => t ()
      | Meta.$ (MetaOperator.NORMAL p, es) => into (p $ Vector.map (unconvert t) es)

  local
    structure Pairs = SplaySet(structure Elem = DuplicateOrdered(Variable))
    open Meta

    fun aequiv pairs (l, r) =
        case (out l, out r) of
            (` v, ` v') => Variable.eq (v, v') orelse Pairs.member pairs (v, v')
          | (x \ e, y \ e') => aequiv (Pairs.insert pairs (x, y)) (e, e')
          | (MetaOperator.WILD $ _, _) => true
          | (_, MetaOperator.WILD $ _) => true
          | (oper $ args, oper' $ args') =>
            Operator.eq (oper, oper')
            andalso Vector.all (aequiv pairs) (VectorPair.zip (args, args'))
          | _ => false
  in
    fun eqModWild (l, r) = aequiv Pairs.empty (l, r)
  end
end
