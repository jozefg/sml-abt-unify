structure Ops =
struct
  datatype t = All | Arr | Unit

  val eq = op=

  fun arity e =
    case e of
        All => #[1]
      | Arr => #[0, 0]
      | Unit => #[]

  fun toString All = "all"
    | toString Arr = "arr"
    | toString Unit = "unit"
end

structure SystemFTests =
struct
  structure AU = AbtUtil(Abt(structure Operator = Ops
                             structure Variable = Variable ()))

  open Ops AU
  structure U = AbtUnify(AU)
  open U
  infix $ \
  infix 8 $$ // \\

  fun assert b = if b then () else raise Fail "assert failed"

  fun test (l, r) =
    let
      val sol = unify (l, r)
      val l' = U.Solution.foldl (fn (v, e', e) => subst e' v e) l sol
      val r' = U.Solution.foldl (fn (v, e', e) => subst e' v e) r sol
    in
      if eq (l', r')
      then true
      else raise Fail "Returned false solution"
    end handle Mismatch _ => false

  val correct = assert o test
  val incorrect = assert o not o test

  val (a, b, c, d) = (Variable.named "a",
                      Variable.named "b",
                      Variable.named "c",
                      Variable.named "d")

  val () = correct (All $$ #[a \\ `` a], All $$ #[b \\ `` b])
  val () = incorrect (All $$ #[a \\ `` a],
                      All $$ #[b \\ (Arr $$ #[`` b, `` b])])
  val () = correct (`` a, All $$ #[b \\ `` b])
  val () = correct (All $$ #[b \\ `` b], `` a)
  val () = incorrect (`` a, All $$ #[b \\ `` a])
  val () = correct (Arr $$ #[`` a, `` a],
                    Arr $$ #[Arr $$ #[`` b, `` b], Arr $$ #[`` b, `` b]])
  val true = matches (All $$ #[a \\ `` b], All $$ #[a \\ `` a])
  val false = matches (All $$ #[a \\ (Arr $$ #[`` a, `` a])], All $$ #[a \\ `` a])
end

structure SystemFMetaTests =
struct
  structure A = MetaAbt(Abt(structure Operator = Ops
                            structure Variable = Variable ()))
  open A
  structure AU = AbtUtil(Meta)

  open Ops MetaOperator
  open AU

  structure U = AbtUnifyOperators(structure A = AU
                                  structure O = MetaOperator)
  open U
  infix $ \
  infix 8 $$ // \\

  fun assert b = if b then () else raise Fail "assert failed"
  fun applySol sol e =
    U.Solution.foldl (fn (v, e', e) => substOperator (fn _ => e') (META v) e) e sol

  fun test (l, r) =
    let
      val sol = unify (l, r)
      val l' = applySol sol l
      val r' = applySol sol r
    in
      if eq (l', r')
      then true
      else raise Fail "Returned false solution"
    end handle Mismatch (l,r) => false

  val correct = assert o test
  val incorrect = assert o not o test

  val (a, b, c, d) = (Variable.named "a",
                      Variable.named "b",
                      Variable.named "c",
                      Variable.named "d")
  fun meta v = META v $$ #[]
  fun all es = NORMAL All $$ es
  fun arr es = NORMAL Arr $$ es

  val () = correct (all #[a \\ `` a], all #[b \\ `` b])
  val () = incorrect (all #[a \\ `` a],
                      all #[b \\ (arr #[`` b, `` b])])
  val () = correct (meta a, all #[b \\ `` b])
  val () = correct (all #[b \\ `` b], meta a)
  val () = incorrect (meta a, all #[b \\ meta a])
  val () = correct (arr #[meta a, meta a],
                    arr #[arr #[meta b, meta b], arr #[meta b, meta b]])
  val true = matches (all #[a \\ meta b],
                      all #[a \\ `` a])
  val false = matches (all #[a \\ (arr #[`` a, `` a])],
                       all #[a \\ `` a])
end
