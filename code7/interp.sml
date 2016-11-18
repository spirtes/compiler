(*  COMP 321 HW 6:  Interpreter for a fragment of C.
*   
*   Norman Danner
*   Fall 2016
*)

structure Interp =
struct

  (*  Fill in the rest of this type definition as you need.
  *)
  datatype value = VNone

  (*  Here is a useful definition.
  *)
  type env = value Env.env

  (*  You must raise the exceptions as appropriate.
  *)
  exception NoMainError
  exception NoReturnError
  exception UninitializedError
  exception RuntimeTypeError

  (*  You must define this function.  It need only be defined for values of
  *   types in the source language (i.e., int, double, bool, string).
  *)
  fun valueToString (v : value) : string =
    raise Fail "Unimplemented"

  (*  You must define this function.  Of course, it will really delegate to a
  *   more general eval function that evaluates an expression under an
  *   environment.
  *)
  fun evalNoEnv (e : AnnAst.exp) : value =
    VNone

  (*  You must define this function.
  *)
  fun exec(p : AnnAst.program) : int =
    0

end
