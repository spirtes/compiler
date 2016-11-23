(*  COMP 321 Homework 7:  Compiler for a fragment of C.
*
*   N. Danner
*   Fall 2016
*)

structure Compile =
struct

  structure A = AnnAst
  structure T = TextIO

  exception Unimplemented

  type locs = int Env.env
  type nextLoc = int
  type nextLabel = int
  type fncs = (A.paramdecl list * A.typ) Env.env

  type compileenv = locs * nextLoc * nextLabel * fncs

  fun writeString(s : string, outs : T.outstream) : unit =
    let val () = T.output(outs,s^"\n") in
     T.flushOut(outs)
    end

  datatype order = Lt | Gt | Ge | Le | Eq | Ne

  fun getFunType(t : A.typ) : string =
    case t of
      A.Tbool => "Z"
    | A.Tint => "I"
    | A.Tdouble => "D"
    | A.Tvoid => "V"
    | _ => "error"

  fun getFunArgs(params : A.paramdecl list) : string =
    case params of
      [] => ""
    | (t,id)::ps => getFunType(t) ^ getFunArgs(ps)

  fun getPrefix(t : A.typ) : string =
    case t of
      A.Tbool => "i"
    | A.Tint => "i"
    | A.Tdouble => "d"
    | _ => raise Unimplemented

  fun getTyp(e : A.exp) : A.typ =
    case e of
      A.EInt(_) => A.Tint
    | A.EDouble(_) => A.Tdouble
    | A.EId(_,t) => t
    | A.ECall((_,_),t) => t
    | A.EPostIncr(_,t) => t
    | A.EPostDecr(_,t) => t
    | A.EPreIncr(_,t) => t
    | A.EPreDecr(_,t) => t
    | A.EMul((_,_),t) => t
    | A.EDiv((_,_),t) => t
    | A.EMod((_,_),t) => t
    | A.EAdd((_,_),t) => t
    | A.ESub((_,_),t) => t
    | A.EAsst((_,_),t) => t
    | A.ECond((_,_,_),t) => t
    | _ => raise Unimplemented

  fun genLabel(n : nextLabel) : string * nextLabel =
    ("LBL" ^ Int.toString(n),n + 1)

  fun compareHelper (t: A.typ, e0: A.exp, e1: A.exp, ord: order, env: compileenv, outs: T.outstream) : compileenv =
    case getTyp(e0) of
    A.Tint => let val (locs,nextLoc,nextLabel,fncs) = env
                  val comp = (case ord of
                                Lt => "if_icmplt "
                              | Gt => "if_icmpgt "
                              | Ge => "if_icmpge "
                              | Le => "if_icmple "
                              | Eq => "if_icmpeq "
                              | Ne => "if_icmpne ")
                  val env1 = compileExp(e0,outs,env)
                  val (locs',nextLoc',nextLabel',fncs') = compileExp(e1,outs,env1)
                  val (truelbl,label0) = genLabel(nextLabel)
                  val a = writeString(comp ^ truelbl, outs)
                  val b = writeString("ldc 0", outs)
                  val (falselbl,label1) = genLabel(label0)
                  val c = writeString("goto " ^ falselbl, outs)
                  val d = writeString(truelbl ^ ":", outs)
                  val e = writeString("ldc 1", outs)
                  val f = writeString(falselbl ^ ":", outs) in
                      (locs',nextLoc',label1,fncs')
                  end
    | A.Tdouble => let val (locs, nextLoc, nextLabel,fncs) = env
                       val env1 = compileExp(e0, outs, env)
                       val (locs', nextLoc', nextLabel',fncs') = compileExp(e1, outs, env1)
                       val a = writeString("dcmpg", outs)
                       val env2 = (case ord of
                                  Lt => let val c = writeString("ldc -1",outs)
                                            val (truelbl,label0) = genLabel(nextLabel)
                                            val a = writeString("if_icmpeq " ^ truelbl, outs)
                                            val b = writeString("ldc 0", outs)
                                            val (falselbl,label1) = genLabel(label0)
                                            val c = writeString("goto " ^ falselbl, outs)
                                            val d = writeString(truelbl ^ ":", outs)
                                            val e = writeString("ldc 1", outs)
                                            val f = writeString(falselbl ^ ":", outs) in
                                                (locs',nextLoc',label1,fncs')
                                            end

                                | Gt => let val c = writeString("ldc 1",outs)
                                          val (truelbl,label0) = genLabel(nextLabel)
                                          val a = writeString("if_icmpeq " ^ truelbl, outs)
                                          val b = writeString("ldc 0", outs)
                                          val (falselbl,label1) = genLabel(label0)
                                          val c = writeString("goto " ^ falselbl, outs)
                                          val d = writeString(truelbl ^ ":", outs)
                                          val e = writeString("ldc 1", outs)
                                          val f = writeString(falselbl ^ ":", outs) in
                                              (locs',nextLoc',label1,fncs')
                                          end
                                | Le => let val c = writeString("ldc 1",outs)
                                          val (truelbl,label0) = genLabel(nextLabel)
                                          val a = writeString("if_icmplt " ^ truelbl, outs)
                                          val b = writeString("ldc 0", outs)
                                          val (falselbl,label1) = genLabel(label0)
                                          val c = writeString("goto " ^ falselbl, outs)
                                          val d = writeString(truelbl ^ ":", outs)
                                          val e = writeString("ldc 1", outs)
                                          val f = writeString(falselbl ^ ":", outs) in
                                              (locs',nextLoc',label1,fncs')
                                          end
                                | Ge => let val c = writeString("ldc -1",outs)
                                          val (truelbl,label0) = genLabel(nextLabel)
                                          val a = writeString("if_icmpgt " ^ truelbl, outs)
                                          val b = writeString("ldc 0", outs)
                                          val (falselbl,label1) = genLabel(label0)
                                          val c = writeString("goto " ^ falselbl, outs)
                                          val d = writeString(truelbl ^ ":", outs)
                                          val e = writeString("ldc 1", outs)
                                          val f = writeString(falselbl ^ ":", outs) in
                                              (locs',nextLoc',label1,fncs')
                                          end
                                | Eq => let val c = writeString("ldc 0",outs)
                                          val (truelbl,label0) = genLabel(nextLabel)
                                          val a = writeString("if_icmpeq " ^ truelbl, outs)
                                          val b = writeString("ldc 0", outs)
                                          val (falselbl,label1) = genLabel(label0)
                                          val c = writeString("goto " ^ falselbl, outs)
                                          val d = writeString(truelbl ^ ":", outs)
                                          val e = writeString("ldc 1", outs)
                                          val f = writeString(falselbl ^ ":", outs) in
                                              (locs',nextLoc',label1,fncs')
                                          end
                                |Ne => let val c = writeString("ldc 0",outs)
                                          val (truelbl,label0) = genLabel(nextLabel)
                                          val a = writeString("if_icmpeq " ^ truelbl, outs)
                                          val b = writeString("ldc 1", outs)
                                          val (falselbl,label1) = genLabel(label0)
                                          val c = writeString("goto " ^ falselbl, outs)
                                          val d = writeString(truelbl ^ ":", outs)
                                          val e = writeString("ldc 0", outs)
                                          val f = writeString(falselbl ^ ":", outs) in
                                              (locs',nextLoc',label1,fncs')
                                          end)
                              in
                                env2
                              end
      | _ => raise Unimplemented

  and compileExp(exp : A.exp, outs: T.outstream, env : compileenv) : compileenv =
    case exp of
      A.EInt(x) => let val a = writeString(("ldc " ^ Int.toString(x)), outs)
                   in
                     env
                   end
    | A.EDouble(x) => let val a = writeString("ldc2_w " ^ Real.toString(x), outs ) in
                        env
                      end
    | A.EString(s) => raise Unimplemented
    | A.ETrue => let val a = writeString("ldc 1", outs) in
                   env
                 end
    | A.EFalse => let val a = writeString("ldc 0", outs) in
                   env
                 end
    | A.EId(id,t) => let val (locs,nextLoc,nextLabel,fncs) = env
                         val idloc = Env.lookup locs id
                         val a = writeString(getPrefix(t) ^ "load " ^ Int.toString(idloc), outs) in
                           env
                      end
    | A.ECall((id,exps),t) => (case id of
                                "readInt" => let val (locs,nextLoc,nextLabel,fncs) = compileExpList(exps,outs,env)
                                                 val a = writeString("invokestatic CSupport/" ^ id
                                                        ^ "()I" ,outs)
                                             in
                                               (locs,nextLoc,nextLabel,fncs)
                                             end
                              | "printInt" => let val (locs,nextLoc,nextLabel,fncs) = compileExpList(exps,outs,env)
                                                  val a = writeString("invokestatic CSupport/" ^ id
                                                      ^ "(I)V" ,outs)
                                                  val () = writeString("ldc 0", outs)
                                              in
                                                (locs,nextLoc,nextLabel,fncs)
                                              end
                              | "readDouble" => let val (locs,nextLoc,nextLabel,fncs) = compileExpList(exps,outs,env)
                                                    val a = writeString("invokestatic CSupport/" ^ id
                                                      ^ "()D" ,outs)
                                                in
                                                  (locs,nextLoc,nextLabel,fncs)
                                                end
                             | "printDouble" => let val (locs,nextLoc,nextLabel,fncs) = compileExpList(exps,outs,env)
                                                 val a = writeString("invokestatic CSupport/" ^ id
                                                        ^ "(D)V" ,outs)
                                                 val () = writeString("ldc 0", outs)
                                             in
                                               (locs,nextLoc,nextLabel,fncs)
                                             end
                              | "readBool" => let val (locs,nextLoc,nextLabel,fncs) = compileExpList(exps,outs,env)
                                                  val a = writeString("invokestatic CSupport/" ^ id
                                                    ^ "()Z" ,outs)
                                              in
                                                (locs,nextLoc,nextLabel,fncs)
                                              end
                              | "printBool" => let val (locs,nextLoc,nextLabel,fncs) = compileExpList(exps,outs,env)
                                                  val a = writeString("invokestatic CSupport/" ^ id
                                                         ^ "(Z)V" ,outs)
                                                  val () = writeString("ldc 0", outs)
                                               in
                                                 (locs,nextLoc,nextLabel,fncs)
                                               end
                              | _ =>
    let val (locs,nextLoc,nextLabel,fncs) = compileExpList(exps,outs,env)
                                  val (params,t) = Env.lookup fncs id
                                  val args = getFunArgs(params)
                                  val typ = getFunType(t)
                                  val a = writeString("invokestatic C/" ^ id
                                                ^ "(" ^ args ^ ")" ^ typ,outs)
                                  val () = (case t of
                                             A.Tvoid => writeString("ldc 0", outs)
                                           | _ => ())
                              in
                                (locs,nextLoc,nextLabel,fncs)
                              end)
    | A.EPreIncr(id,t) => let val (locs,nextLoc,nextLabel,fncs) = env
                               val idloc = Env.lookup locs id
                               val a = writeString(getPrefix(t) ^ "load " ^ Int.toString(idloc),outs)
                               val b = writeString("ldc 1",outs)
                               val c = writeString("iadd",outs)
                               val d = writeString("dup",outs)
                               val e = writeString("istore " ^ Int.toString(idloc),outs)
                            in
                              env
                            end
    | A.EPreDecr(id,t) => let val (locs,nextLoc,nextLabel,fncs) = env
                               val idloc = Env.lookup locs id
                               val a = writeString(getPrefix(t) ^ "load " ^ Int.toString(idloc),outs)
                               val b = writeString("ldc 1",outs)
                               val c = writeString("isub",outs)
                               val d = writeString("dup",outs)
                               val e = writeString("istore " ^ Int.toString(idloc),outs)
                            in
                              env
                            end
    | A.ENot(e,t) => let val (locs,nextLoc,nextLabel,fncs) = compileExp(e,outs,env)
                         val (falselbl,label0) = genLabel(nextLabel)
                         val a = writeString("ifeq " ^ falselbl,outs)
                         val c = writeString("ldc 0",outs)
                         val (truelbl,label1) = genLabel(label0)
                         val d = writeString("goto " ^ truelbl,outs)
                         val e = writeString(falselbl ^ ":",outs)
                         val g = writeString("ldc 1",outs)
                         val h = writeString(truelbl ^ ":",outs)
                      in
                        (locs,nextLoc,label1,fncs)
                      end
    | A.EPostIncr(id,t) => let val (locs,nextLoc,nextLabel,fncs) = env
                               val idloc = Env.lookup locs id
                               val a = writeString(getPrefix(t) ^ "load " ^ Int.toString(idloc),outs)
                               val d = writeString("dup",outs)
                               val b = writeString("ldc 1",outs)
                               val c = writeString("iadd",outs)
                               val e = writeString("istore " ^ Int.toString(idloc),outs)
                            in
                              env
                            end
    | A.EPostDecr(id,t) => let val (locs,nextLoc,nextLabel,fncs) = env
                               val idloc = Env.lookup locs id
                               val a = writeString(getPrefix(t) ^ "load " ^ Int.toString(idloc),outs)
                               val d = writeString("dup",outs)
                               val b = writeString("ldc 1",outs)
                               val c = writeString("isub",outs)
                               val e = writeString("istore " ^ Int.toString(idloc),outs)
                            in
                              env
                            end
    | A.EMul((e0,e1),t) => let val env1 = compileExp(e0,outs,env)
                               val env2 = compileExp(e1,outs,env1)
                               val a = writeString(getPrefix(t) ^ "mul",outs) in
                             env2
                           end
    | A.EDiv((e0,e1),t) => let val env1 = compileExp(e0,outs,env)
                               val env2 = compileExp(e1,outs,env1)
                               val a = writeString(getPrefix(t) ^ "div",outs) in
                             env2
                           end
    | A.EMod((e0,e1),t) => let val env1 = compileExp(e0,outs,env)
                               val env2 = compileExp(e1,outs,env1)
                               val a = writeString(getPrefix(t) ^ "rem",outs) in
                             env2
                           end
    | A.EAdd((e0,e1),t) => let val env1 = compileExp(e0,outs,env)
                               val env2 = compileExp(e1,outs,env1)
                               val a = writeString(getPrefix(t) ^ "add",outs) in
                             env2
                           end
    | A.ESub((e0,e1),t) => let val env1 = compileExp(e0,outs,env)
                               val env2 = compileExp(e1,outs,env1)
                               val a = writeString(getPrefix(t) ^ "sub",outs) in
                             env2
                           end
    | A.ELt((e0,e1),t) => compareHelper(t, e0, e1, Lt, env, outs)
    | A.EGt((e0,e1),t) => compareHelper(t, e0, e1, Gt, env, outs)
    | A.ELe((e0,e1),t) => compareHelper(t, e0, e1, Le, env, outs)
    | A.EGe((e0,e1),t) => compareHelper(t, e0, e1, Ge, env, outs)
    | A.EEq((e0,e1),t) => compareHelper(t, e0, e1, Eq, env, outs)
    | A.ENeq((e0,e1),t) => compareHelper(t, e0, e1, Ne, env, outs)
    | A.EAnd((e0,e1),t) => let val env1 = compileExp(e0,outs,env)
                               val env2 = compileExp(e1,outs,env1)
                               val a = writeString("iand",outs) in
                             env2
                           end
    | A.EOr((e0,e1),t) => let val env1 = compileExp(e0,outs,env)
                               val env2 = compileExp(e1,outs,env1)
                               val a = writeString("ior",outs) in
                             env2
                           end
    | A.EAsst((id,e),t) => let val (locs,nextLoc,nextLabel,fncs) = compileExp(e,outs,env)
                               val a = (case t of
                                          A.Tdouble => writeString("dup2",outs)
                                        | _ => writeString("dup",outs))
                               val idloc = Env.lookup locs id
                               val b = writeString(getPrefix(t) ^ "store " ^ Int.toString(idloc),outs)
                           in
                             (locs,nextLoc,nextLabel,fncs)
                           end
    | A.ECond((e0,e1,e2),t) => let val (locs,nextLoc,nextLabel,fncs) = compileExp(e0,outs,env)
                                   val (falselbl,label0) = genLabel(nextLabel)
                                   val a = writeString("ifeq " ^ falselbl,outs)
                                   val (locs',nextLoc',nextLabel',fncs') = compileExp(e1,outs,(locs,nextLoc,label0,fncs))
                                   val (truelbl,label1) = genLabel(nextLabel')
                                   val b = writeString("goto " ^ truelbl,outs)
                                   val c = writeString(falselbl ^ ":",outs)
                                   val env'' = compileExp(e2,outs,(locs',nextLoc',label1,fncs'))
                                   val d = writeString(truelbl ^ ":",outs)
                                in
                                  env''
                                end

  and compileExpList(exps : A.exp list, outs : T.outstream, env : compileenv) : compileenv =
    case exps of
      [] => env
    | e::es => let val env' = compileExp(e,outs,env)
               in
                 compileExpList(es,outs,env')
               end


  fun incNextLoc(n : nextLoc, t : A.typ) : nextLoc =
    case t of
      A.Tbool => n + 1
    | A.Tint => n + 1
    | A.Tdouble => n + 2
    | _ => n


  fun compileStm (stm : A.stm, outs : T.outstream, env : compileenv, returnt : A.typ option) : compileenv =
    let val (locs,nextLoc,nextLabel,fncs) = env in
    case stm of
      A.SExp(e) => let val newenv = compileExp(e,outs,env)
                       val () = (case getTyp(e) of
                                   A.Tdouble => writeString("pop2",outs)
                                 | _ => writeString("pop",outs))
                   in
                     newenv
                   end
    | A.SDecl(t,ids) => (case ids of
                           [] => env
                        |  id::xs => let val newloc = Env.extend locs id nextLoc
                        in
                          compileStm(A.SDecl(t,xs),outs,
                                    (newloc,incNextLoc(nextLoc,t),nextLabel,fncs),returnt)
                        end)

    | A.SInit(t,idexplist) => (case idexplist of
                                   [] => env
                                 | (id,e)::xs => let val (locs',nextLoc',nextLabel',fncs') =
                                                          compileExp(e,outs,env)
                                                     val newloc = Env.extend locs' id nextLoc'
                                                     val b = writeString(getPrefix(t) ^ "store "
                                                              ^ (Int.toString(Env.lookup newloc id)),outs)
                                                 in
                                                   compileStm(A.SInit(t,xs),outs,
                                                              (newloc,incNextLoc(nextLoc',t),nextLabel',fncs'),returnt)
                                                  end)
    | A.SDoWhile(s,e) => let val (locs,nextLoc,nextLabel,fncs) = env
                           val (testlbl,label0) = genLabel(nextLabel)
                           val a = writeString(testlbl ^ ":",outs)
                           val env' = compileStm(s,outs,(locs,nextLoc,label0,fncs),returnt)
                           val (locs',nextLoc',nextLabel',fncs') = compileExp(e,outs,env')
                           val (endlbl,label1) = genLabel(nextLabel')
                           val b = writeString("ifeq " ^ endlbl,outs)
                           val c = writeString("goto " ^ testlbl,outs)
                           val d = writeString(endlbl ^ ":",outs)
                        in
                          (locs',nextLoc',label1,fncs')
                        end
    | A.SWhile(e,s) => let val (locs,nextLoc,nextLabel,fncs) = env
                           val (testlbl,label0) = genLabel(nextLabel)
                           val a = writeString(testlbl ^ ":",outs)
                           val (locs',nextLoc',nextLabel',fncs') =
                                compileExp(e,outs,(locs,nextLoc,label0,fncs))
                           val (endlbl,label1) = genLabel(nextLabel')
                           val b = writeString("ifeq " ^ endlbl,outs)
                           val env' = compileStm(s,outs,
                                        (locs',nextLoc',label1,fncs'),returnt)
                           val c = writeString("goto " ^ testlbl,outs)
                           val d = writeString(endlbl ^ ":",outs)
                        in
                          env'
                        end

    | A.SFor((t,id,e0),e1,e2,s) => let val (locs',nextLoc',nextLabel',fncs') =
                                            compileExp(e0,outs,env)
                                       val newloc = Env.extend locs' id nextLoc'
                                       val b = writeString(getPrefix(t) ^ "store "
                                                ^ (Int.toString(Env.lookup newloc id)),outs)
                                       val (truelbl,label0) = genLabel(nextLabel')
                                       val c = writeString(truelbl ^ ":",outs)
                                       val (locs'',nextLoc'',nextLabel'',fncs'') =
                                            compileExp(e1,outs,(newloc,incNextLoc(nextLoc',t),label0,fncs'))
                                       val (falselbl,label1) = genLabel(nextLabel'')
                                       val d = writeString("ifeq " ^ falselbl, outs)
                                       val env' = compileStm(s,outs,(locs'',nextLoc'',label1,fncs''),returnt)
                                       val env'' = compileExp(e2,outs,env')
                                       val () = writeString("pop",outs)
                                       val e = writeString("goto " ^ truelbl,outs)
                                       val f = writeString(falselbl ^ ":",outs)
                                    in
                                      env''
                                    end
    | A.SIf(e,s) => let val (locs,nextLoc,nextLabel,fncs) = compileExp(e,outs,env)
                        val (falselbl,label0) = genLabel(nextLabel)
                        val a  = writeString("ifeq " ^ falselbl,outs)
                        val env' = compileStm(s,outs,(locs,nextLoc,label0,fncs),returnt)
                        val b = writeString(falselbl ^ ":",outs)
                    in
                      env'
                    end
    | A.SIfElse(e,s0,s1) => let val (locs,nextLoc,nextLabel,fncs) = compileExp(e,outs,env)
                                val (falselbl,label0) = genLabel(nextLabel)
                                val a  = writeString("ifeq " ^ falselbl,outs)
                                val (locs',nextLoc',nextLabel',fncs') = compileStm(s0,outs,(locs,nextLoc,label0,fncs),returnt)
                                val (truelbl,label1) = genLabel(nextLabel')
                                val b = writeString("goto " ^ truelbl,outs)
                                val c = writeString(falselbl ^ ":",outs)
                                val env' = compileStm(s1,outs,(locs',nextLoc',label1,fncs'),returnt)
                                val d = writeString(truelbl ^ ":",outs)
                            in
                              env'
                            end
    | A.SReturn(e) => (case returnt of
                         NONE => let val () = writeString("return",outs)
                                 in
                                   env
                                 end
                       | SOME(t) => let val env' = compileExp(e,outs,env)
                                        val () = writeString(getPrefix(t) ^ "return",outs)
                                    in
                                      env'
                                    end)
    | A.SVReturn => let val a = writeString("return",outs)
                    in
                      env
                    end
    | A.SBlock(slist) => let val (locs,nextLoc,nextLabel,fncs) = env
                             val (locs',nextLoc',nextLabel',fncs') =
                                  compileStmList(slist,outs,
                                      ((Env.pushFrame locs Frame.empty)
                                        ,nextLoc,nextLabel,fncs),returnt)
                         in
                           ((Env.popFrame locs'),nextLoc,nextLabel,fncs)
                         end
    end

  and compileStmList(stms : A.stm list, outs : T.outstream,
                env : compileenv, returnt : A.typ option) : compileenv =
    case stms of
      [] => env
    | s::ss => let val newenv = compileStm(s,outs,env,returnt)
               in
                 compileStmList(ss,outs,newenv,returnt)
               end

  fun updateEnv(env : compileenv, params : A.paramdecl list) : compileenv =
    let val (locs,nextLoc,nextLabel,fncs) = env
    in
      case params of
        [] => env
        | (t,id)::ps => updateEnv(((Env.extend locs id nextLoc,
                                    incNextLoc(nextLoc,t),nextLabel,fncs),ps))
    end

  fun compileDef(d : A.def, outs : T.outstream, env : compileenv) : compileenv =
      case d of
        A.DFun(t,id,params,stms) =>
  let val newenv = updateEnv(env,params)
      val (locs,nextLoc,nextLabel,fncs) = newenv
      val fncs' = Env.extend fncs id (params,t)
      val args = getFunArgs(params)
      val typ = getFunType(t)
      val () = (case id of
                 "main" => writeString((".method public static main([Ljava/lang/String;)V"),outs)
               | _ => writeString((".method public static " ^
                                    id ^ "(" ^ args ^
                                    ")" ^ typ),outs))
      val () = writeString((".limit locals 1000"), outs)
      val () = writeString((".limit stack 1000"), outs)
      val (_,_,nextLabel,fncs'') = (case id of
                                      "main" => compileStmList(stms,outs,(locs,nextLoc,nextLabel,fncs'),NONE)
                                    | _ => compileStmList(stms,outs,(locs,nextLoc,nextLabel,fncs'),SOME(t)))
      val () = writeString("nop",outs)
      val () = writeString((".end method"), outs)
      in
        ((Env.pushFrame Env.empty Frame.empty),0,nextLabel,fncs'')
      end
      | _ => env


  fun compileDefList(defs : A.def list, outs : T.outstream, env : compileenv) : compileenv =
    case defs of
      [] => env
    | d::ds => let val env' = compileDef(d,outs,env)
               in
                 compileDefList(ds,outs,env')
               end

  (*  compile(p, outs) = ().  As a side-effect, p is compiled to Jasmin
  *   assembly, with the resulting assembly code written to the output
  *   stream outs.  The client is responsible for opening outs before calling
  *   this function and closing it after this function returns.
  *)
  fun compile(A.PDefs(deflist) : A.program, outs : T.outstream) : unit =
    let val () = writeString(".class public C",outs)
        val () = writeString(".super java/lang/Object",outs)
        val () = writeString("",outs)
        val () = writeString(".method public <init>()V",outs)
        val () = writeString("aload_0",outs)
        val () = writeString("invokenonvirtual java/lang/Object/<init>()V",outs)
        val () = writeString("return",outs)
        val () = writeString(".end method",outs)
        val env = compileDefList(deflist, outs,((Env.pushFrame Env.empty Frame.empty)
                                ,0,0,Env.pushFrame Env.empty Frame.empty))
    in
      ()
    end

end
