(* cfg-memory-pickle.sml
 *
 * Generated from cfg.asdl by asdlgen.
 *)

structure LambdaVarMemoryPickle : LAMBDA_VAR_PICKLE
  where type instream = ASDLMemoryPickle.instream
  where type outstream = ASDLMemoryPickle.outstream = struct
    (*---------- begin streams.in ----------*)
    type instream = ASDLMemoryPickle.instream
    type outstream = ASDLMemoryPickle.outstream
(*---------- end streams.in ----------*)

    val write_type = LambdaVarPickle.write_type ASDLMemoryPickle.output1
    val read_type = LambdaVarPickle.read_type ASDLMemoryPickle.input1
    val write_lvar = LambdaVarPickle.write_lvar ASDLMemoryPickle.output1
    val read_lvar = LambdaVarPickle.read_lvar ASDLMemoryPickle.input1
  end

structure CTypesMemoryPickle : CTYPES_PICKLE
  where type instream = ASDLMemoryPickle.instream
  where type outstream = ASDLMemoryPickle.outstream = struct
    (*---------- begin pickle-util.in ----------*)
    type instream = ASDLMemoryPickle.instream
    type outstream = ASDLMemoryPickle.outstream
  (* write an option *)
    fun writeOption wrFn (outS, NONE) = ASDLMemoryPickle.writeTag8(outS, 0w0)
      | writeOption wrFn (outS, SOME obj) = (
          ASDLMemoryPickle.writeTag8(outS, 0w1);
          wrFn(outS, obj))
  (* read an option *)
    fun readOption rdFn inS = (case ASDLMemoryPickle.readTag8 inS
           of 0w0 => NONE
            | 0w1 => let val obj = rdFn in SOME obj end
            | _ => raise ASDL.DecodeError
          (* end case *))
  (* write a list of values as a sequence *)
    fun writeSeq wrFn (outS, xs) = let
          fun write [] = ()
            | write (x::xr) = (wrFn(outS, x); write xr)
          in
            ASDLMemoryPickle.writeUInt(outS, Word.fromInt(length xs));
            write xs
          end
  (* read a sequence into a sequence of values *)
    fun readSeq rdFn inS = let
          val len = ASDLMemoryPickle.readUInt inS
          fun read (0w0, xs) = reverse (xs, [])
            | read (n, xs) = let
                val x = rdFn inS
                in
                  read (n-0w1, x::xs)
                end
          and reverse ([], xs) = xs
            | reverse (x::xr, xs) = reverse (xr, x::xs)
          in
            read (len, [])
          end
(*---------- end pickle-util.in ----------*)

    fun write_calling_convention (outS, obj) = ASDLMemoryPickle.write_string (outS, obj)
    fun read_calling_convention inS = ASDLMemoryPickle.read_string inS
    fun write_c_int (outS, obj) = (case obj
           of CTypes.I_char => ASDLMemoryPickle.write_tag8 (outS, 0w1)
            | CTypes.I_short => ASDLMemoryPickle.write_tag8 (outS, 0w2)
            | CTypes.I_int => ASDLMemoryPickle.write_tag8 (outS, 0w3)
            | CTypes.I_long => ASDLMemoryPickle.write_tag8 (outS, 0w4)
            | CTypes.I_long_long => ASDLMemoryPickle.write_tag8 (outS, 0w5))
    fun read_c_int inS = (case ASDLMemoryPickle.read_tag8 inS
           of 0w1 => CTypes.I_char
            | 0w2 => CTypes.I_short
            | 0w3 => CTypes.I_int
            | 0w4 => CTypes.I_long
            | 0w5 => CTypes.I_long_long
            | _ => raise ASDL.DecodeError)
    fun write_c_type (outS, obj) = (case obj
           of CTypes.C_void => ASDLMemoryPickle.write_tag8 (outS, 0w1)
            | CTypes.C_float => ASDLMemoryPickle.write_tag8 (outS, 0w2)
            | CTypes.C_double => ASDLMemoryPickle.write_tag8 (outS, 0w3)
            | CTypes.C_long_double => ASDLMemoryPickle.write_tag8 (outS, 0w4)
            | CTypes.C_unsigned x0 => (
              ASDLMemoryPickle.write_tag8 (outS, 0w5);
              write_c_int (outS, x0))
            | CTypes.C_signed x0 => (
              ASDLMemoryPickle.write_tag8 (outS, 0w6);
              write_c_int (outS, x0))
            | CTypes.C_PTR => ASDLMemoryPickle.write_tag8 (outS, 0w7)
            | CTypes.C_ARRAY(x0, x1) => (
              ASDLMemoryPickle.write_tag8 (outS, 0w8);
              write_c_type (outS, x0);
              ASDLMemoryPickle.write_int (outS, x1))
            | CTypes.C_STRUCT x0 => (
              ASDLMemoryPickle.write_tag8 (outS, 0w9);
              writeSeq write_c_type (outS, x0))
            | CTypes.C_UNION x0 => (
              ASDLMemoryPickle.write_tag8 (outS, 0w10);
              writeSeq write_c_type (outS, x0)))
    fun read_c_type inS = (case ASDLMemoryPickle.read_tag8 inS
           of 0w1 => CTypes.C_void
            | 0w2 => CTypes.C_float
            | 0w3 => CTypes.C_double
            | 0w4 => CTypes.C_long_double
            | 0w5 => let val x0 = read_c_int inS in CTypes.C_unsigned (x0) end
            | 0w6 => let val x0 = read_c_int inS in CTypes.C_signed (x0) end
            | 0w7 => CTypes.C_PTR
            | 0w8 => let
              val x0 = read_c_type inS
              val x1 = ASDLMemoryPickle.read_int inS
              in
                  CTypes.C_ARRAY (x0, x1)
              end
            | 0w9 => let val x0 = readSeq read_c_type inS in CTypes.C_STRUCT (x0) end
            | 0w10 => let val x0 = readSeq read_c_type inS in CTypes.C_UNION (x0) end
            | _ => raise ASDL.DecodeError)
    fun write_c_proto (outS, obj) = let
          val {conv, retTy, paramTys} = obj
          in
            write_calling_convention (outS, conv);
            write_c_type (outS, retTy);
            writeSeq write_c_type (outS, paramTys)
          end
    fun read_c_proto inS = let
          val conv = read_calling_convention inS
          val retTy = read_c_type inS
          val paramTys = readSeq read_c_type inS
          in
              {conv = conv, retTy = retTy, paramTys = paramTys}
          end
  end

structure CFG_PrimMemoryPickle : CFG__PRIM_PICKLE
  where type instream = ASDLMemoryPickle.instream
  where type outstream = ASDLMemoryPickle.outstream = struct
    (*---------- begin pickle-util.in ----------*)
    type instream = ASDLMemoryPickle.instream
    type outstream = ASDLMemoryPickle.outstream
  (* write an option *)
    fun writeOption wrFn (outS, NONE) = ASDLMemoryPickle.writeTag8(outS, 0w0)
      | writeOption wrFn (outS, SOME obj) = (
          ASDLMemoryPickle.writeTag8(outS, 0w1);
          wrFn(outS, obj))
  (* read an option *)
    fun readOption rdFn inS = (case ASDLMemoryPickle.readTag8 inS
           of 0w0 => NONE
            | 0w1 => let val obj = rdFn in SOME obj end
            | _ => raise ASDL.DecodeError
          (* end case *))
  (* write a list of values as a sequence *)
    fun writeSeq wrFn (outS, xs) = let
          fun write [] = ()
            | write (x::xr) = (wrFn(outS, x); write xr)
          in
            ASDLMemoryPickle.writeUInt(outS, Word.fromInt(length xs));
            write xs
          end
  (* read a sequence into a sequence of values *)
    fun readSeq rdFn inS = let
          val len = ASDLMemoryPickle.readUInt inS
          fun read (0w0, xs) = reverse (xs, [])
            | read (n, xs) = let
                val x = rdFn inS
                in
                  read (n-0w1, x::xs)
                end
          and reverse ([], xs) = xs
            | reverse (x::xr, xs) = reverse (xr, x::xs)
          in
            read (len, [])
          end
(*---------- end pickle-util.in ----------*)

    fun write_fcmpop (outS, obj) = (case obj
           of CFG_Prim.F_EQ => ASDLMemoryPickle.write_tag8 (outS, 0w1)
            | CFG_Prim.F_ULG => ASDLMemoryPickle.write_tag8 (outS, 0w2)
            | CFG_Prim.F_UN => ASDLMemoryPickle.write_tag8 (outS, 0w3)
            | CFG_Prim.F_LEG => ASDLMemoryPickle.write_tag8 (outS, 0w4)
            | CFG_Prim.F_GT => ASDLMemoryPickle.write_tag8 (outS, 0w5)
            | CFG_Prim.F_GE => ASDLMemoryPickle.write_tag8 (outS, 0w6)
            | CFG_Prim.F_UGT => ASDLMemoryPickle.write_tag8 (outS, 0w7)
            | CFG_Prim.F_UGE => ASDLMemoryPickle.write_tag8 (outS, 0w8)
            | CFG_Prim.F_LT => ASDLMemoryPickle.write_tag8 (outS, 0w9)
            | CFG_Prim.F_LE => ASDLMemoryPickle.write_tag8 (outS, 0w10)
            | CFG_Prim.F_ULT => ASDLMemoryPickle.write_tag8 (outS, 0w11)
            | CFG_Prim.F_ULE => ASDLMemoryPickle.write_tag8 (outS, 0w12)
            | CFG_Prim.F_LG => ASDLMemoryPickle.write_tag8 (outS, 0w13)
            | CFG_Prim.F_UE => ASDLMemoryPickle.write_tag8 (outS, 0w14))
    fun read_fcmpop inS = (case ASDLMemoryPickle.read_tag8 inS
           of 0w1 => CFG_Prim.F_EQ
            | 0w2 => CFG_Prim.F_ULG
            | 0w3 => CFG_Prim.F_UN
            | 0w4 => CFG_Prim.F_LEG
            | 0w5 => CFG_Prim.F_GT
            | 0w6 => CFG_Prim.F_GE
            | 0w7 => CFG_Prim.F_UGT
            | 0w8 => CFG_Prim.F_UGE
            | 0w9 => CFG_Prim.F_LT
            | 0w10 => CFG_Prim.F_LE
            | 0w11 => CFG_Prim.F_ULT
            | 0w12 => CFG_Prim.F_ULE
            | 0w13 => CFG_Prim.F_LG
            | 0w14 => CFG_Prim.F_UE
            | _ => raise ASDL.DecodeError)
    fun write_cmpop (outS, obj) = (case obj
           of CFG_Prim.GT => ASDLMemoryPickle.write_tag8 (outS, 0w1)
            | CFG_Prim.GTE => ASDLMemoryPickle.write_tag8 (outS, 0w2)
            | CFG_Prim.LT => ASDLMemoryPickle.write_tag8 (outS, 0w3)
            | CFG_Prim.LTE => ASDLMemoryPickle.write_tag8 (outS, 0w4)
            | CFG_Prim.EQL => ASDLMemoryPickle.write_tag8 (outS, 0w5)
            | CFG_Prim.NEQ => ASDLMemoryPickle.write_tag8 (outS, 0w6))
    fun read_cmpop inS = (case ASDLMemoryPickle.read_tag8 inS
           of 0w1 => CFG_Prim.GT
            | 0w2 => CFG_Prim.GTE
            | 0w3 => CFG_Prim.LT
            | 0w4 => CFG_Prim.LTE
            | 0w5 => CFG_Prim.EQL
            | 0w6 => CFG_Prim.NEQ
            | _ => raise ASDL.DecodeError)
    fun write_branch (outS, obj) = (case obj
           of CFG_Prim.CMP{oper, signed, sz} => (
              ASDLMemoryPickle.write_tag8 (outS, 0w1);
              write_cmpop (outS, oper);
              ASDLMemoryPickle.write_bool (outS, signed);
              ASDLMemoryPickle.write_int (outS, sz))
            | CFG_Prim.FCMP{oper, sz} => (
              ASDLMemoryPickle.write_tag8 (outS, 0w2);
              write_fcmpop (outS, oper);
              ASDLMemoryPickle.write_int (outS, sz))
            | CFG_Prim.FSGN x0 => (
              ASDLMemoryPickle.write_tag8 (outS, 0w3);
              ASDLMemoryPickle.write_int (outS, x0))
            | CFG_Prim.PEQL => ASDLMemoryPickle.write_tag8 (outS, 0w4)
            | CFG_Prim.PNEQ => ASDLMemoryPickle.write_tag8 (outS, 0w5))
    fun read_branch inS = (case ASDLMemoryPickle.read_tag8 inS
           of 0w1 => let
              val oper = read_cmpop inS
              val signed = ASDLMemoryPickle.read_bool inS
              val sz = ASDLMemoryPickle.read_int inS
              in
                  CFG_Prim.CMP {oper = oper, signed = signed, sz = sz}
              end
            | 0w2 => let
              val oper = read_fcmpop inS
              val sz = ASDLMemoryPickle.read_int inS
              in
                  CFG_Prim.FCMP {oper = oper, sz = sz}
              end
            | 0w3 => let val x0 = ASDLMemoryPickle.read_int inS in CFG_Prim.FSGN (x0) end
            | 0w4 => CFG_Prim.PEQL
            | 0w5 => CFG_Prim.PNEQ
            | _ => raise ASDL.DecodeError)
    fun write_numkind (outS, obj) = (case obj
           of CFG_Prim.INT => ASDLMemoryPickle.write_tag8 (outS, 0w1)
            | CFG_Prim.FLT => ASDLMemoryPickle.write_tag8 (outS, 0w2))
    fun read_numkind inS = (case ASDLMemoryPickle.read_tag8 inS
           of 0w1 => CFG_Prim.INT
            | 0w2 => CFG_Prim.FLT
            | _ => raise ASDL.DecodeError)
    fun write_setter (outS, obj) = (case obj
           of CFG_Prim.UNBOXED_UPDATE => ASDLMemoryPickle.write_tag8 (outS, 0w1)
            | CFG_Prim.UPDATE => ASDLMemoryPickle.write_tag8 (outS, 0w2)
            | CFG_Prim.UNBOXED_ASSIGN => ASDLMemoryPickle.write_tag8 (outS, 0w3)
            | CFG_Prim.ASSIGN => ASDLMemoryPickle.write_tag8 (outS, 0w4)
            | CFG_Prim.RAW_UPDATE{kind, sz} => (
              ASDLMemoryPickle.write_tag8 (outS, 0w5);
              write_numkind (outS, kind);
              ASDLMemoryPickle.write_int (outS, sz))
            | CFG_Prim.RAW_STORE{kind, sz} => (
              ASDLMemoryPickle.write_tag8 (outS, 0w6);
              write_numkind (outS, kind);
              ASDLMemoryPickle.write_int (outS, sz))
            | CFG_Prim.SET_HDLR => ASDLMemoryPickle.write_tag8 (outS, 0w7)
            | CFG_Prim.SET_VAR => ASDLMemoryPickle.write_tag8 (outS, 0w8))
    fun read_setter inS = (case ASDLMemoryPickle.read_tag8 inS
           of 0w1 => CFG_Prim.UNBOXED_UPDATE
            | 0w2 => CFG_Prim.UPDATE
            | 0w3 => CFG_Prim.UNBOXED_ASSIGN
            | 0w4 => CFG_Prim.ASSIGN
            | 0w5 => let
              val kind = read_numkind inS
              val sz = ASDLMemoryPickle.read_int inS
              in
                  CFG_Prim.RAW_UPDATE {kind = kind, sz = sz}
              end
            | 0w6 => let
              val kind = read_numkind inS
              val sz = ASDLMemoryPickle.read_int inS
              in
                  CFG_Prim.RAW_STORE {kind = kind, sz = sz}
              end
            | 0w7 => CFG_Prim.SET_HDLR
            | 0w8 => CFG_Prim.SET_VAR
            | _ => raise ASDL.DecodeError)
    fun write_looker (outS, obj) = (case obj
           of CFG_Prim.DEREF => ASDLMemoryPickle.write_tag8 (outS, 0w1)
            | CFG_Prim.SUBSCRIPT => ASDLMemoryPickle.write_tag8 (outS, 0w2)
            | CFG_Prim.RAW_SUBSCRIPT{kind, sz} => (
              ASDLMemoryPickle.write_tag8 (outS, 0w3);
              write_numkind (outS, kind);
              ASDLMemoryPickle.write_int (outS, sz))
            | CFG_Prim.RAW_LOAD{kind, sz} => (
              ASDLMemoryPickle.write_tag8 (outS, 0w4);
              write_numkind (outS, kind);
              ASDLMemoryPickle.write_int (outS, sz))
            | CFG_Prim.GET_HDLR => ASDLMemoryPickle.write_tag8 (outS, 0w5)
            | CFG_Prim.GET_VAR => ASDLMemoryPickle.write_tag8 (outS, 0w6))
    fun read_looker inS = (case ASDLMemoryPickle.read_tag8 inS
           of 0w1 => CFG_Prim.DEREF
            | 0w2 => CFG_Prim.SUBSCRIPT
            | 0w3 => let
              val kind = read_numkind inS
              val sz = ASDLMemoryPickle.read_int inS
              in
                  CFG_Prim.RAW_SUBSCRIPT {kind = kind, sz = sz}
              end
            | 0w4 => let
              val kind = read_numkind inS
              val sz = ASDLMemoryPickle.read_int inS
              in
                  CFG_Prim.RAW_LOAD {kind = kind, sz = sz}
              end
            | 0w5 => CFG_Prim.GET_HDLR
            | 0w6 => CFG_Prim.GET_VAR
            | _ => raise ASDL.DecodeError)
    fun write_pureop (outS, obj) = (case obj
           of CFG_Prim.ADD => ASDLMemoryPickle.write_tag8 (outS, 0w1)
            | CFG_Prim.SUB => ASDLMemoryPickle.write_tag8 (outS, 0w2)
            | CFG_Prim.SMUL => ASDLMemoryPickle.write_tag8 (outS, 0w3)
            | CFG_Prim.SDIV => ASDLMemoryPickle.write_tag8 (outS, 0w4)
            | CFG_Prim.SREM => ASDLMemoryPickle.write_tag8 (outS, 0w5)
            | CFG_Prim.UMUL => ASDLMemoryPickle.write_tag8 (outS, 0w6)
            | CFG_Prim.UDIV => ASDLMemoryPickle.write_tag8 (outS, 0w7)
            | CFG_Prim.UREM => ASDLMemoryPickle.write_tag8 (outS, 0w8)
            | CFG_Prim.LSHIFT => ASDLMemoryPickle.write_tag8 (outS, 0w9)
            | CFG_Prim.RSHIFT => ASDLMemoryPickle.write_tag8 (outS, 0w10)
            | CFG_Prim.RSHIFTL => ASDLMemoryPickle.write_tag8 (outS, 0w11)
            | CFG_Prim.ORB => ASDLMemoryPickle.write_tag8 (outS, 0w12)
            | CFG_Prim.XORB => ASDLMemoryPickle.write_tag8 (outS, 0w13)
            | CFG_Prim.ANDB => ASDLMemoryPickle.write_tag8 (outS, 0w14)
            | CFG_Prim.FADD => ASDLMemoryPickle.write_tag8 (outS, 0w15)
            | CFG_Prim.FSUB => ASDLMemoryPickle.write_tag8 (outS, 0w16)
            | CFG_Prim.FMUL => ASDLMemoryPickle.write_tag8 (outS, 0w17)
            | CFG_Prim.FDIV => ASDLMemoryPickle.write_tag8 (outS, 0w18)
            | CFG_Prim.FNEG => ASDLMemoryPickle.write_tag8 (outS, 0w19)
            | CFG_Prim.FABS => ASDLMemoryPickle.write_tag8 (outS, 0w20)
            | CFG_Prim.FSQRT => ASDLMemoryPickle.write_tag8 (outS, 0w21))
    fun read_pureop inS = (case ASDLMemoryPickle.read_tag8 inS
           of 0w1 => CFG_Prim.ADD
            | 0w2 => CFG_Prim.SUB
            | 0w3 => CFG_Prim.SMUL
            | 0w4 => CFG_Prim.SDIV
            | 0w5 => CFG_Prim.SREM
            | 0w6 => CFG_Prim.UMUL
            | 0w7 => CFG_Prim.UDIV
            | 0w8 => CFG_Prim.UREM
            | 0w9 => CFG_Prim.LSHIFT
            | 0w10 => CFG_Prim.RSHIFT
            | 0w11 => CFG_Prim.RSHIFTL
            | 0w12 => CFG_Prim.ORB
            | 0w13 => CFG_Prim.XORB
            | 0w14 => CFG_Prim.ANDB
            | 0w15 => CFG_Prim.FADD
            | 0w16 => CFG_Prim.FSUB
            | 0w17 => CFG_Prim.FMUL
            | 0w18 => CFG_Prim.FDIV
            | 0w19 => CFG_Prim.FNEG
            | 0w20 => CFG_Prim.FABS
            | 0w21 => CFG_Prim.FSQRT
            | _ => raise ASDL.DecodeError)
    fun write_pure (outS, obj) = (case obj
           of CFG_Prim.PURE_ARITH{oper, sz} => (
              ASDLMemoryPickle.write_tag8 (outS, 0w1);
              write_pureop (outS, oper);
              ASDLMemoryPickle.write_int (outS, sz))
            | CFG_Prim.EXTEND{signed, from, to} => (
              ASDLMemoryPickle.write_tag8 (outS, 0w2);
              ASDLMemoryPickle.write_bool (outS, signed);
              ASDLMemoryPickle.write_int (outS, from);
              ASDLMemoryPickle.write_int (outS, to))
            | CFG_Prim.INT_TO_REAL{from, to} => (
              ASDLMemoryPickle.write_tag8 (outS, 0w3);
              ASDLMemoryPickle.write_int (outS, from);
              ASDLMemoryPickle.write_int (outS, to))
            | CFG_Prim.PURE_SUBSCRIPT => ASDLMemoryPickle.write_tag8 (outS, 0w4)
            | CFG_Prim.PURE_RAW_SUBSCRIPT{kind, sz} => (
              ASDLMemoryPickle.write_tag8 (outS, 0w5);
              write_numkind (outS, kind);
              ASDLMemoryPickle.write_int (outS, sz)))
    fun read_pure inS = (case ASDLMemoryPickle.read_tag8 inS
           of 0w1 => let
              val oper = read_pureop inS
              val sz = ASDLMemoryPickle.read_int inS
              in
                  CFG_Prim.PURE_ARITH {oper = oper, sz = sz}
              end
            | 0w2 => let
              val signed = ASDLMemoryPickle.read_bool inS
              val from = ASDLMemoryPickle.read_int inS
              val to = ASDLMemoryPickle.read_int inS
              in
                  CFG_Prim.EXTEND {signed = signed, from = from, to = to}
              end
            | 0w3 => let
              val from = ASDLMemoryPickle.read_int inS
              val to = ASDLMemoryPickle.read_int inS
              in
                  CFG_Prim.INT_TO_REAL {from = from, to = to}
              end
            | 0w4 => CFG_Prim.PURE_SUBSCRIPT
            | 0w5 => let
              val kind = read_numkind inS
              val sz = ASDLMemoryPickle.read_int inS
              in
                  CFG_Prim.PURE_RAW_SUBSCRIPT {kind = kind, sz = sz}
              end
            | _ => raise ASDL.DecodeError)
    fun write_arithop (outS, obj) = (case obj
           of CFG_Prim.IADD => ASDLMemoryPickle.write_tag8 (outS, 0w1)
            | CFG_Prim.ISUB => ASDLMemoryPickle.write_tag8 (outS, 0w2)
            | CFG_Prim.IMUL => ASDLMemoryPickle.write_tag8 (outS, 0w3)
            | CFG_Prim.IDIV => ASDLMemoryPickle.write_tag8 (outS, 0w4)
            | CFG_Prim.IREM => ASDLMemoryPickle.write_tag8 (outS, 0w5))
    fun read_arithop inS = (case ASDLMemoryPickle.read_tag8 inS
           of 0w1 => CFG_Prim.IADD
            | 0w2 => CFG_Prim.ISUB
            | 0w3 => CFG_Prim.IMUL
            | 0w4 => CFG_Prim.IDIV
            | 0w5 => CFG_Prim.IREM
            | _ => raise ASDL.DecodeError)
    fun write_rounding_mode (outS, obj) = (case obj
           of CFG_Prim.TO_NEAREST => ASDLMemoryPickle.write_tag8 (outS, 0w1)
            | CFG_Prim.TO_NEGINF => ASDLMemoryPickle.write_tag8 (outS, 0w2)
            | CFG_Prim.TO_POSINF => ASDLMemoryPickle.write_tag8 (outS, 0w3)
            | CFG_Prim.TO_ZERO => ASDLMemoryPickle.write_tag8 (outS, 0w4))
    fun read_rounding_mode inS = (case ASDLMemoryPickle.read_tag8 inS
           of 0w1 => CFG_Prim.TO_NEAREST
            | 0w2 => CFG_Prim.TO_NEGINF
            | 0w3 => CFG_Prim.TO_POSINF
            | 0w4 => CFG_Prim.TO_ZERO
            | _ => raise ASDL.DecodeError)
    fun write_arith (outS, obj) = (case obj
           of CFG_Prim.ARITH{oper, sz} => (
              ASDLMemoryPickle.write_tag8 (outS, 0w1);
              write_arithop (outS, oper);
              ASDLMemoryPickle.write_int (outS, sz))
            | CFG_Prim.REAL_TO_INT{mode, from, to} => (
              ASDLMemoryPickle.write_tag8 (outS, 0w2);
              write_rounding_mode (outS, mode);
              ASDLMemoryPickle.write_int (outS, from);
              ASDLMemoryPickle.write_int (outS, to)))
    fun read_arith inS = (case ASDLMemoryPickle.read_tag8 inS
           of 0w1 => let
              val oper = read_arithop inS
              val sz = ASDLMemoryPickle.read_int inS
              in
                  CFG_Prim.ARITH {oper = oper, sz = sz}
              end
            | 0w2 => let
              val mode = read_rounding_mode inS
              val from = ASDLMemoryPickle.read_int inS
              val to = ASDLMemoryPickle.read_int inS
              in
                  CFG_Prim.REAL_TO_INT {mode = mode, from = from, to = to}
              end
            | _ => raise ASDL.DecodeError)
    fun write_alloc (outS, obj) = (case obj
           of CFG_Prim.RECORD{desc, mut} => (
              ASDLMemoryPickle.write_tag8 (outS, 0w1);
              ASDLMemoryPickle.write_integer (outS, desc);
              ASDLMemoryPickle.write_bool (outS, mut))
            | CFG_Prim.RAW_RECORD{desc, kind, sz} => (
              ASDLMemoryPickle.write_tag8 (outS, 0w2);
              ASDLMemoryPickle.write_integer (outS, desc);
              write_numkind (outS, kind);
              ASDLMemoryPickle.write_int (outS, sz))
            | CFG_Prim.RAW_ALLOC{desc, align, len} => (
              ASDLMemoryPickle.write_tag8 (outS, 0w3);
              writeOption ASDLMemoryPickle.write_integer (outS, desc);
              ASDLMemoryPickle.write_int (outS, align);
              ASDLMemoryPickle.write_int (outS, len)))
    fun read_alloc inS = (case ASDLMemoryPickle.read_tag8 inS
           of 0w1 => let
              val desc = ASDLMemoryPickle.read_integer inS
              val mut = ASDLMemoryPickle.read_bool inS
              in
                  CFG_Prim.RECORD {desc = desc, mut = mut}
              end
            | 0w2 => let
              val desc = ASDLMemoryPickle.read_integer inS
              val kind = read_numkind inS
              val sz = ASDLMemoryPickle.read_int inS
              in
                  CFG_Prim.RAW_RECORD {desc = desc, kind = kind, sz = sz}
              end
            | 0w3 => let
              val desc = readOption ASDLMemoryPickle.read_integer inS
              val align = ASDLMemoryPickle.read_int inS
              val len = ASDLMemoryPickle.read_int inS
              in
                  CFG_Prim.RAW_ALLOC {desc = desc, align = align, len = len}
              end
            | _ => raise ASDL.DecodeError)
  end

structure CFGMemoryPickle : CFGPICKLE
  where type instream = ASDLMemoryPickle.instream
  where type outstream = ASDLMemoryPickle.outstream = struct
    (*---------- begin pickle-util.in ----------*)
    type instream = ASDLMemoryPickle.instream
    type outstream = ASDLMemoryPickle.outstream
  (* write an option *)
    fun writeOption wrFn (outS, NONE) = ASDLMemoryPickle.writeTag8(outS, 0w0)
      | writeOption wrFn (outS, SOME obj) = (
          ASDLMemoryPickle.writeTag8(outS, 0w1);
          wrFn(outS, obj))
  (* read an option *)
    fun readOption rdFn inS = (case ASDLMemoryPickle.readTag8 inS
           of 0w0 => NONE
            | 0w1 => let val obj = rdFn in SOME obj end
            | _ => raise ASDL.DecodeError
          (* end case *))
  (* write a list of values as a sequence *)
    fun writeSeq wrFn (outS, xs) = let
          fun write [] = ()
            | write (x::xr) = (wrFn(outS, x); write xr)
          in
            ASDLMemoryPickle.writeUInt(outS, Word.fromInt(length xs));
            write xs
          end
  (* read a sequence into a sequence of values *)
    fun readSeq rdFn inS = let
          val len = ASDLMemoryPickle.readUInt inS
          fun read (0w0, xs) = reverse (xs, [])
            | read (n, xs) = let
                val x = rdFn inS
                in
                  read (n-0w1, x::xs)
                end
          and reverse ([], xs) = xs
            | reverse (x::xr, xs) = reverse (xr, x::xs)
          in
            read (len, [])
          end
(*---------- end pickle-util.in ----------*)

    fun write_attrs (outS, obj) = let
          val {alignHP, needsBasePtr, hasTrapArith, hasRCC} = obj
          in
            ASDLMemoryPickle.write_int (outS, alignHP);
            ASDLMemoryPickle.write_bool (outS, needsBasePtr);
            ASDLMemoryPickle.write_bool (outS, hasTrapArith);
            ASDLMemoryPickle.write_bool (outS, hasRCC)
          end
    fun read_attrs inS = let
          val alignHP = ASDLMemoryPickle.read_int inS
          val needsBasePtr = ASDLMemoryPickle.read_bool inS
          val hasTrapArith = ASDLMemoryPickle.read_bool inS
          val hasRCC = ASDLMemoryPickle.read_bool inS
          in
              {
              alignHP = alignHP,
              needsBasePtr = needsBasePtr,
              hasTrapArith = hasTrapArith,
              hasRCC = hasRCC}
          end
    fun write_probability (outS, obj) = ASDLMemoryPickle.write_int (outS, obj)
    fun read_probability inS = ASDLMemoryPickle.read_int inS
    fun write_ty (outS, obj) = (case obj
           of CFG.NUMt x0 => (
              ASDLMemoryPickle.write_tag8 (outS, 0w1);
              ASDLMemoryPickle.write_int (outS, x0))
            | CFG.FLTt x0 => (
              ASDLMemoryPickle.write_tag8 (outS, 0w2);
              ASDLMemoryPickle.write_int (outS, x0))
            | CFG.PTRt => ASDLMemoryPickle.write_tag8 (outS, 0w3)
            | CFG.FUNt => ASDLMemoryPickle.write_tag8 (outS, 0w4)
            | CFG.CNTt => ASDLMemoryPickle.write_tag8 (outS, 0w5))
    fun read_ty inS = (case ASDLMemoryPickle.read_tag8 inS
           of 0w1 => let val x0 = ASDLMemoryPickle.read_int inS in CFG.NUMt (x0) end
            | 0w2 => let val x0 = ASDLMemoryPickle.read_int inS in CFG.FLTt (x0) end
            | 0w3 => CFG.PTRt
            | 0w4 => CFG.FUNt
            | 0w5 => CFG.CNTt
            | _ => raise ASDL.DecodeError)
    fun write_param (outS, obj) = let
          val (x0, x1) = obj
          in
            LambdaVarMemoryPickle.write_lvar (outS, x0);
            write_ty (outS, x1)
          end
    fun read_param inS = let
          val x0 = LambdaVarMemoryPickle.read_lvar inS
          val x1 = read_ty inS
          in
              (x0, x1)
          end
    fun write_exp (outS, obj) = (case obj
           of CFG.VAR x0 => (
              ASDLMemoryPickle.write_tag8 (outS, 0w1);
              LambdaVarMemoryPickle.write_lvar (outS, x0))
            | CFG.LABEL x0 => (
              ASDLMemoryPickle.write_tag8 (outS, 0w2);
              LambdaVarMemoryPickle.write_lvar (outS, x0))
            | CFG.NUM{iv, signed, sz} => (
              ASDLMemoryPickle.write_tag8 (outS, 0w3);
              ASDLMemoryPickle.write_integer (outS, iv);
              ASDLMemoryPickle.write_bool (outS, signed);
              ASDLMemoryPickle.write_int (outS, sz))
            | CFG.LOOKER(x0, x1) => (
              ASDLMemoryPickle.write_tag8 (outS, 0w4);
              CFG_PrimMemoryPickle.write_looker (outS, x0);
              writeSeq write_exp (outS, x1))
            | CFG.PURE(x0, x1) => (
              ASDLMemoryPickle.write_tag8 (outS, 0w5);
              CFG_PrimMemoryPickle.write_pure (outS, x0);
              writeSeq write_exp (outS, x1))
            | CFG.SELECT(x0, x1) => (
              ASDLMemoryPickle.write_tag8 (outS, 0w6);
              ASDLMemoryPickle.write_int (outS, x0);
              write_exp (outS, x1))
            | CFG.OFFSET(x0, x1) => (
              ASDLMemoryPickle.write_tag8 (outS, 0w7);
              ASDLMemoryPickle.write_int (outS, x0);
              write_exp (outS, x1)))
    fun read_exp inS = (case ASDLMemoryPickle.read_tag8 inS
           of 0w1 => let val x0 = LambdaVarMemoryPickle.read_lvar inS in CFG.VAR (x0) end
            | 0w2 => let
              val x0 = LambdaVarMemoryPickle.read_lvar inS
              in CFG.LABEL (x0)
              end
            | 0w3 => let
              val iv = ASDLMemoryPickle.read_integer inS
              val signed = ASDLMemoryPickle.read_bool inS
              val sz = ASDLMemoryPickle.read_int inS
              in
                  CFG.NUM {iv = iv, signed = signed, sz = sz}
              end
            | 0w4 => let
              val x0 = CFG_PrimMemoryPickle.read_looker inS
              val x1 = readSeq read_exp inS
              in
                  CFG.LOOKER (x0, x1)
              end
            | 0w5 => let
              val x0 = CFG_PrimMemoryPickle.read_pure inS
              val x1 = readSeq read_exp inS
              in
                  CFG.PURE (x0, x1)
              end
            | 0w6 => let
              val x0 = ASDLMemoryPickle.read_int inS
              val x1 = read_exp inS
              in
                  CFG.SELECT (x0, x1)
              end
            | 0w7 => let
              val x0 = ASDLMemoryPickle.read_int inS
              val x1 = read_exp inS
              in
                  CFG.OFFSET (x0, x1)
              end
            | _ => raise ASDL.DecodeError)
    fun write_calling_conv (outS, obj) = (case obj
           of CFG.STD_FUN => ASDLMemoryPickle.write_tag8 (outS, 0w1)
            | CFG.STD_CONT => ASDLMemoryPickle.write_tag8 (outS, 0w2)
            | CFG.KNOWN_CHK => ASDLMemoryPickle.write_tag8 (outS, 0w3)
            | CFG.KNOWN => ASDLMemoryPickle.write_tag8 (outS, 0w4))
    fun read_calling_conv inS = (case ASDLMemoryPickle.read_tag8 inS
           of 0w1 => CFG.STD_FUN
            | 0w2 => CFG.STD_CONT
            | 0w3 => CFG.KNOWN_CHK
            | 0w4 => CFG.KNOWN
            | _ => raise ASDL.DecodeError)
    fun write_stm (outS, obj) = (case obj
           of CFG.LET(x0, x1, x2) => (
              ASDLMemoryPickle.write_tag8 (outS, 0w1);
              write_exp (outS, x0);
              write_param (outS, x1);
              write_stm (outS, x2))
            | CFG.ALLOC(x0, x1, x2, x3) => (
              ASDLMemoryPickle.write_tag8 (outS, 0w2);
              CFG_PrimMemoryPickle.write_alloc (outS, x0);
              writeSeq write_exp (outS, x1);
              LambdaVarMemoryPickle.write_lvar (outS, x2);
              write_stm (outS, x3))
            | CFG.APPLY(x0, x1) => (
              ASDLMemoryPickle.write_tag8 (outS, 0w3);
              writeSeq write_exp (outS, x0);
              writeSeq write_ty (outS, x1))
            | CFG.THROW(x0, x1) => (
              ASDLMemoryPickle.write_tag8 (outS, 0w4);
              writeSeq write_exp (outS, x0);
              writeSeq write_ty (outS, x1))
            | CFG.GOTO(x0, x1, x2, x3) => (
              ASDLMemoryPickle.write_tag8 (outS, 0w5);
              write_calling_conv (outS, x0);
              LambdaVarMemoryPickle.write_lvar (outS, x1);
              writeSeq write_exp (outS, x2);
              writeSeq write_ty (outS, x3))
            | CFG.SWITCH(x0, x1) => (
              ASDLMemoryPickle.write_tag8 (outS, 0w6);
              write_exp (outS, x0);
              writeSeq write_stm (outS, x1))
            | CFG.BRANCH(x0, x1, x2, x3, x4) => (
              ASDLMemoryPickle.write_tag8 (outS, 0w7);
              CFG_PrimMemoryPickle.write_branch (outS, x0);
              writeSeq write_exp (outS, x1);
              write_probability (outS, x2);
              write_stm (outS, x3);
              write_stm (outS, x4))
            | CFG.STREQL(x0, x1, x2, x3, x4) => (
              ASDLMemoryPickle.write_tag8 (outS, 0w8);
              ASDLMemoryPickle.write_int (outS, x0);
              write_exp (outS, x1);
              write_exp (outS, x2);
              write_stm (outS, x3);
              write_stm (outS, x4))
            | CFG.ARITH(x0, x1, x2, x3) => (
              ASDLMemoryPickle.write_tag8 (outS, 0w9);
              CFG_PrimMemoryPickle.write_arith (outS, x0);
              writeSeq write_exp (outS, x1);
              write_param (outS, x2);
              write_stm (outS, x3))
            | CFG.SETTER(x0, x1, x2) => (
              ASDLMemoryPickle.write_tag8 (outS, 0w10);
              CFG_PrimMemoryPickle.write_setter (outS, x0);
              writeSeq write_exp (outS, x1);
              write_stm (outS, x2))
            | CFG.RCC{reentrant, linkage, proto, args, results, live, k} => (
              ASDLMemoryPickle.write_tag8 (outS, 0w11);
              ASDLMemoryPickle.write_bool (outS, reentrant);
              ASDLMemoryPickle.write_string (outS, linkage);
              CTypesMemoryPickle.write_c_proto (outS, proto);
              writeSeq write_exp (outS, args);
              writeSeq write_param (outS, results);
              writeSeq write_param (outS, live);
              write_stm (outS, k)))
    fun read_stm inS = (case ASDLMemoryPickle.read_tag8 inS
           of 0w1 => let
              val x0 = read_exp inS
              val x1 = read_param inS
              val x2 = read_stm inS
              in
                  CFG.LET (x0, x1, x2)
              end
            | 0w2 => let
              val x0 = CFG_PrimMemoryPickle.read_alloc inS
              val x1 = readSeq read_exp inS
              val x2 = LambdaVarMemoryPickle.read_lvar inS
              val x3 = read_stm inS
              in
                  CFG.ALLOC (x0, x1, x2, x3)
              end
            | 0w3 => let
              val x0 = readSeq read_exp inS
              val x1 = readSeq read_ty inS
              in
                  CFG.APPLY (x0, x1)
              end
            | 0w4 => let
              val x0 = readSeq read_exp inS
              val x1 = readSeq read_ty inS
              in
                  CFG.THROW (x0, x1)
              end
            | 0w5 => let
              val x0 = read_calling_conv inS
              val x1 = LambdaVarMemoryPickle.read_lvar inS
              val x2 = readSeq read_exp inS
              val x3 = readSeq read_ty inS
              in
                  CFG.GOTO (x0, x1, x2, x3)
              end
            | 0w6 => let
              val x0 = read_exp inS
              val x1 = readSeq read_stm inS
              in
                  CFG.SWITCH (x0, x1)
              end
            | 0w7 => let
              val x0 = CFG_PrimMemoryPickle.read_branch inS
              val x1 = readSeq read_exp inS
              val x2 = read_probability inS
              val x3 = read_stm inS
              val x4 = read_stm inS
              in
                  CFG.BRANCH (x0, x1, x2, x3, x4)
              end
            | 0w8 => let
              val x0 = ASDLMemoryPickle.read_int inS
              val x1 = read_exp inS
              val x2 = read_exp inS
              val x3 = read_stm inS
              val x4 = read_stm inS
              in
                  CFG.STREQL (x0, x1, x2, x3, x4)
              end
            | 0w9 => let
              val x0 = CFG_PrimMemoryPickle.read_arith inS
              val x1 = readSeq read_exp inS
              val x2 = read_param inS
              val x3 = read_stm inS
              in
                  CFG.ARITH (x0, x1, x2, x3)
              end
            | 0w10 => let
              val x0 = CFG_PrimMemoryPickle.read_setter inS
              val x1 = readSeq read_exp inS
              val x2 = read_stm inS
              in
                  CFG.SETTER (x0, x1, x2)
              end
            | 0w11 => let
              val reentrant = ASDLMemoryPickle.read_bool inS
              val linkage = ASDLMemoryPickle.read_string inS
              val proto = CTypesMemoryPickle.read_c_proto inS
              val args = readSeq read_exp inS
              val results = readSeq read_param inS
              val live = readSeq read_param inS
              val k = read_stm inS
              in
                  CFG.RCC
                  {
                    reentrant = reentrant,
                    linkage = linkage,
                    proto = proto,
                    args = args,
                    results = results,
                    live = live,
                    k = k}
              end
            | _ => raise ASDL.DecodeError)
    fun write_frag (outS, obj) = let
          val CFG.Frag{gcCheck, lab, params, body} = obj
          in
            ASDLMemoryPickle.write_bool (outS, gcCheck);
            LambdaVarMemoryPickle.write_lvar (outS, lab);
            writeSeq write_param (outS, params);
            write_stm (outS, body)
          end
    fun read_frag inS = let
          val gcCheck = ASDLMemoryPickle.read_bool inS
          val lab = LambdaVarMemoryPickle.read_lvar inS
          val params = readSeq read_param inS
          val body = read_stm inS
          in
              CFG.Frag {gcCheck = gcCheck, lab = lab, params = params, body = body}
          end
    fun write_entry (outS, obj) = let
          val CFG.Entry{cc, lab, params, body} = obj
          in
            write_calling_conv (outS, cc);
            LambdaVarMemoryPickle.write_lvar (outS, lab);
            writeSeq write_param (outS, params);
            write_stm (outS, body)
          end
    fun read_entry inS = let
          val cc = read_calling_conv inS
          val lab = LambdaVarMemoryPickle.read_lvar inS
          val params = readSeq read_param inS
          val body = read_stm inS
          in
              CFG.Entry {cc = cc, lab = lab, params = params, body = body}
          end
    fun write_cluster (outS, obj) = let
          val CFG.Cluster{attrs, entry, frags} = obj
          in
            write_attrs (outS, attrs);
            write_entry (outS, entry);
            writeSeq write_frag (outS, frags)
          end
    fun read_cluster inS = let
          val attrs = read_attrs inS
          val entry = read_entry inS
          val frags = readSeq read_frag inS
          in
              CFG.Cluster {attrs = attrs, entry = entry, frags = frags}
          end
    fun write_comp_unit (outS, obj) = let
          val {srcFile, entry, fns} = obj
          in
            ASDLMemoryPickle.write_string (outS, srcFile);
            write_cluster (outS, entry);
            writeSeq write_cluster (outS, fns)
          end
    fun read_comp_unit inS = let
          val srcFile = ASDLMemoryPickle.read_string inS
          val entry = read_cluster inS
          val fns = readSeq read_cluster inS
          in
              {srcFile = srcFile, entry = entry, fns = fns}
          end
  end

