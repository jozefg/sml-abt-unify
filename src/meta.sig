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

    val convert : A.t -> Meta.t
end
