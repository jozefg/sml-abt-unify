structure SystemFTests =
struct
  structure Ops =
  struct
    datatype t = All | Arr | Unit

    val eq = op=

    fun arity e =
      case e of
          All => #[1]
        | Arr => #[0, 0]
        | Unit => #[]

    fun toString _ = ""
  end

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
      val l' = List.foldl (fn ((v, e'), e) => subst e' v e) l sol
      val r' = List.foldl (fn ((v, e'), e) => subst e' v e) r sol
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
