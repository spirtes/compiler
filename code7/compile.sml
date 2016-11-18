(*  COMP 321 Homework 7:  Compiler for a fragment of C.
*Lex Spirtes 
*)

structure Compile =
struct

  structure A = AnnAst
  structure T = TextIO

  exception Unimplemented

  (*  compile(p, outs) = ().  As a side-effect, p is compiled to Jasmin
  *   assembly, with the resulting assembly code written to the output
  *   stream outs.  The client is responsible for opening outs before calling
  *   this function and closing it after this function returns.
  *)
  fun compile(p : A.program, outs : T.outstream) : unit =
    raise Unimplemented

end
