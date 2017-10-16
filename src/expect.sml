structure Expect =
struct
  datatype expectation = Pass | Fail of string * string

  local
    fun failEq b a =
      Fail ("Expected: " ^ b, "Got: " ^ a)
    
    fun failExn b a =
      Fail ("Expected: " ^ b, "Raised: " ^ a)

    fun exnName (e: exn): string = General.exnName e
  in
    fun truthy true = Pass
      | truthy _    = failEq "true" "false"

    fun falsy false = Pass
      | falsy _     = failEq "false" "true"

    fun equalTo b a = 
      if a = b
      then Pass
      else failEq (PolyML.makestring b) (PolyML.makestring a)

    fun cmp binop b a =
      if binop (a, b)
      then Pass
      else failEq (Int.toString b) (Int.toString a)

    val greaterThan = cmp op>
    val lessThan = cmp op<
    val greaterThanOrEqualTo= cmp op>=
    val lessThanOrEqualTo = cmp op<=

    val atMost = lessThanOrEqualTo
    val atLeast = greaterThanOrEqualTo

    fun nearTo b a =
      if Real.== (b, a)
      then Pass
      else failEq (Real.toString b) (Real.toString a)

    fun none NONE = Pass
      | none _    = Fail ("", "")

    fun some NONE = Fail ("", "")
      | some _    = Pass

    fun anyError f =
      (
        f ();
        failExn "an exception" "Nothing"
      ) handle _ => Pass

    fun error e f =
      (
        f ();
        failExn (exnName e) "Nothing"
      ) handle e' => if exnMessage e' = exnMessage e
                     then Pass
                     else failExn (exnMessage e) (exnMessage e')

    fun all [] _ = Pass
      | all (exp :: exps) x =
        case exp x of
          Pass => all exps x
        | Fail _ => Fail ("", "")
  end
end
