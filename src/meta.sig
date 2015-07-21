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

    (* Converts and [A.t] into a [Meta.t] by turning each
     * operator into the corresponding [NORMAL] operator.
     * This otherwise leaves the rest of the structure intact.
     *)
    val convert : A.t -> Meta.t

    (* This converts an ABT to the corresponding meta-ABT
     * and converts all free variables to the appropriate [META]
     * operator. This means that any term created by [convertFree]
     * should be closed.
     *)
    val convertFree : A.t -> Meta.t

    (* Convert back into a normal [A.t] by exchanging each [META] for
     * a free variable.
     *)
    val unconvert : Meta.t -> A.t
end
