signature META_OPERATOR =
sig
    structure Operator : OPERATOR
    structure Variable : VARIABLE

    datatype t = META of Variable.t | WILD |  NORMAL of Operator.t

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
     * a free variable. [WILD]s are replaced by exchanging each [WILD]
     * for the result of applying the supplied function.
     *)
    val unconvert : (unit -> A.t) -> Meta.t -> A.t

    (* Returns true if the trees are equal if wild cards are considered
     * equal to everything
     *)
    val eqModWild : Meta.t * Meta.t -> bool
end
