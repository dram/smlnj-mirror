// cfg.cxx
//
// Generated from cfg.asdl by asdlgen.
//

#include "cfg.hxx"

namespace CTypes {
    c_type * c_type::read (asdl::instream & is)
    {
        _tag_t tag = static_cast<_tag_t>(asdl::read_tag8(is));
        switch (tag) {
          case _con_C_void:
            return new C_void;
          case _con_C_float:
            return new C_float;
          case _con_C_double:
            return new C_double;
          case _con_C_long_double:
            return new C_long_double;
          case _con_C_unsigned:
            {
                auto f0 = read_c_int(is);
                return new C_unsigned(f0);
            }
          case _con_C_signed:
            {
                auto f0 = read_c_int(is);
                return new C_signed(f0);
            }
          case _con_C_PTR:
            return new C_PTR;
          case _con_C_ARRAY:
            {
                auto f0 = c_type::read_c_type(is);
                auto f1 = asdl::read_int(is);
                return new C_ARRAY(f0, f1);
            }
          case _con_C_STRUCT:
            {
                auto f0 = read_c_type_seq(is);
                return new C_STRUCT(f0);
            }
          case _con_C_UNION:
            {
                auto f0 = read_c_type_seq(is);
                return new C_UNION(f0);
            }
        }
    }
    c_type::~c_type () { }
    C_void::~C_void () { }
    C_float::~C_float () { }
    C_double::~C_double () { }
    C_long_double::~C_long_double () { }
    C_unsigned::~C_unsigned () { }
    C_signed::~C_signed () { }
    C_PTR::~C_PTR () { }
    C_ARRAY::~C_ARRAY ()
    {
        delete this->_v0;
    }
    C_STRUCT::~C_STRUCT () { }
    C_UNION::~C_UNION () { }
    // c_type_seq pickler suppressed
    std::vector<c_type *> read_c_type_seq (asdl::instream & is)
    {
        return asdl::read_seq<c_type>(is);
    }
    // pickler suppressed for c_int
    c_int read_c_int (asdl::instream & is)
    {
        return static_cast<c_int>(asdl::read_tag8(is));
    }
    // pickler suppressed for calling_convention
    calling_convention read_calling_convention (asdl::instream & is)
    {
        auto v = asdl::read_string(is);
        return v;
    }
    c_proto * c_proto::read (asdl::instream & is)
    {
        auto fconv = read_calling_convention(is);
        auto fretTy = c_type::read_c_type(is);
        auto fparamTys = read_c_type_seq(is);
        return new c_proto(fconv, fretTy, fparamTys);
    }
    c_proto::~c_proto ()
    {
        delete this->_v_retTy;
    }
} // namespace CTypes
namespace CFG_Prim {
    // pickler suppressed for numkind
    numkind read_numkind (asdl::instream & is)
    {
        return static_cast<numkind>(asdl::read_tag8(is));
    }
    // pickler suppressed for rounding_mode
    rounding_mode read_rounding_mode (asdl::instream & is)
    {
        return static_cast<rounding_mode>(asdl::read_tag8(is));
    }
    alloc * alloc::read (asdl::instream & is)
    {
        _tag_t tag = static_cast<_tag_t>(asdl::read_tag8(is));
        switch (tag) {
          case _con_RECORD:
            {
                auto fdesc = asdl::read_integer(is);
                auto fmut = asdl::read_bool(is);
                return new RECORD(fdesc, fmut);
            }
          case _con_RAW_RECORD:
            {
                auto fdesc = asdl::read_integer(is);
                auto fkind = read_numkind(is);
                auto fsz = asdl::read_int(is);
                return new RAW_RECORD(fdesc, fkind, fsz);
            }
          case _con_RAW_ALLOC:
            {
                auto fdesc = asdl::read_integer_option(is);
                auto falign = asdl::read_int(is);
                auto flen = asdl::read_int(is);
                return new RAW_ALLOC(fdesc, falign, flen);
            }
        }
    }
    alloc::~alloc () { }
    RECORD::~RECORD () { }
    RAW_RECORD::~RAW_RECORD () { }
    RAW_ALLOC::~RAW_ALLOC () { }
    // pickler suppressed for arithop
    arithop read_arithop (asdl::instream & is)
    {
        return static_cast<arithop>(asdl::read_tag8(is));
    }
    arith * arith::read (asdl::instream & is)
    {
        _tag_t tag = static_cast<_tag_t>(asdl::read_tag8(is));
        switch (tag) {
          case _con_ARITH:
            {
                auto foper = read_arithop(is);
                auto fsz = asdl::read_int(is);
                return new ARITH(foper, fsz);
            }
          case _con_TEST:
            {
                auto ffrom = asdl::read_int(is);
                auto fto = asdl::read_int(is);
                return new TEST(ffrom, fto);
            }
          case _con_TESTU:
            {
                auto ffrom = asdl::read_int(is);
                auto fto = asdl::read_int(is);
                return new TESTU(ffrom, fto);
            }
          case _con_REAL_TO_INT:
            {
                auto fmode = read_rounding_mode(is);
                auto ffrom = asdl::read_int(is);
                auto fto = asdl::read_int(is);
                return new REAL_TO_INT(fmode, ffrom, fto);
            }
        }
    }
    arith::~arith () { }
    ARITH::~ARITH () { }
    TEST::~TEST () { }
    TESTU::~TESTU () { }
    REAL_TO_INT::~REAL_TO_INT () { }
    // pickler suppressed for pureop
    pureop read_pureop (asdl::instream & is)
    {
        return static_cast<pureop>(asdl::read_tag8(is));
    }
    pure * pure::read (asdl::instream & is)
    {
        _tag_t tag = static_cast<_tag_t>(asdl::read_tag8(is));
        switch (tag) {
          case _con_PURE_ARITH:
            {
                auto foper = read_pureop(is);
                auto fsz = asdl::read_int(is);
                return new PURE_ARITH(foper, fsz);
            }
          case _con_EXTEND:
            {
                auto fsigned = asdl::read_bool(is);
                auto ffrom = asdl::read_int(is);
                auto fto = asdl::read_int(is);
                return new EXTEND(fsigned, ffrom, fto);
            }
          case _con_INT_TO_REAL:
            {
                auto ffrom = asdl::read_int(is);
                auto fto = asdl::read_int(is);
                return new INT_TO_REAL(ffrom, fto);
            }
          case _con_PURE_SUBSCRIPT:
            return new PURE_SUBSCRIPT;
          case _con_PURE_RAW_SUBSCRIPT:
            {
                auto fkind = read_numkind(is);
                auto fsz = asdl::read_int(is);
                return new PURE_RAW_SUBSCRIPT(fkind, fsz);
            }
        }
    }
    pure::~pure () { }
    PURE_ARITH::~PURE_ARITH () { }
    EXTEND::~EXTEND () { }
    INT_TO_REAL::~INT_TO_REAL () { }
    PURE_SUBSCRIPT::~PURE_SUBSCRIPT () { }
    PURE_RAW_SUBSCRIPT::~PURE_RAW_SUBSCRIPT () { }
    looker * looker::read (asdl::instream & is)
    {
        _tag_t tag = static_cast<_tag_t>(asdl::read_tag8(is));
        switch (tag) {
          case _con_DEREF:
            return new DEREF;
          case _con_SUBSCRIPT:
            return new SUBSCRIPT;
          case _con_RAW_SUBSCRIPT:
            {
                auto fkind = read_numkind(is);
                auto fsz = asdl::read_int(is);
                return new RAW_SUBSCRIPT(fkind, fsz);
            }
          case _con_RAW_LOAD:
            {
                auto fkind = read_numkind(is);
                auto fsz = asdl::read_int(is);
                return new RAW_LOAD(fkind, fsz);
            }
          case _con_GET_HDLR:
            return new GET_HDLR;
          case _con_GET_VAR:
            return new GET_VAR;
        }
    }
    looker::~looker () { }
    DEREF::~DEREF () { }
    SUBSCRIPT::~SUBSCRIPT () { }
    RAW_SUBSCRIPT::~RAW_SUBSCRIPT () { }
    RAW_LOAD::~RAW_LOAD () { }
    GET_HDLR::~GET_HDLR () { }
    GET_VAR::~GET_VAR () { }
    setter * setter::read (asdl::instream & is)
    {
        _tag_t tag = static_cast<_tag_t>(asdl::read_tag8(is));
        switch (tag) {
          case _con_UNBOXED_UPDATE:
            return new UNBOXED_UPDATE;
          case _con_UPDATE:
            return new UPDATE;
          case _con_UNBOXED_ASSIGN:
            return new UNBOXED_ASSIGN;
          case _con_ASSIGN:
            return new ASSIGN;
          case _con_RAW_UPDATE:
            {
                auto fkind = read_numkind(is);
                auto fsz = asdl::read_int(is);
                return new RAW_UPDATE(fkind, fsz);
            }
          case _con_RAW_STORE:
            {
                auto fkind = read_numkind(is);
                auto fsz = asdl::read_int(is);
                return new RAW_STORE(fkind, fsz);
            }
          case _con_SET_HDLR:
            return new SET_HDLR;
          case _con_SET_VAR:
            return new SET_VAR;
        }
    }
    setter::~setter () { }
    UNBOXED_UPDATE::~UNBOXED_UPDATE () { }
    UPDATE::~UPDATE () { }
    UNBOXED_ASSIGN::~UNBOXED_ASSIGN () { }
    ASSIGN::~ASSIGN () { }
    RAW_UPDATE::~RAW_UPDATE () { }
    RAW_STORE::~RAW_STORE () { }
    SET_HDLR::~SET_HDLR () { }
    SET_VAR::~SET_VAR () { }
    // pickler suppressed for cmpop
    cmpop read_cmpop (asdl::instream & is)
    {
        return static_cast<cmpop>(asdl::read_tag8(is));
    }
    // pickler suppressed for fcmpop
    fcmpop read_fcmpop (asdl::instream & is)
    {
        return static_cast<fcmpop>(asdl::read_tag8(is));
    }
    branch * branch::read (asdl::instream & is)
    {
        _tag_t tag = static_cast<_tag_t>(asdl::read_tag8(is));
        switch (tag) {
          case _con_CMP:
            {
                auto foper = read_cmpop(is);
                auto fsigned = asdl::read_bool(is);
                auto fsz = asdl::read_int(is);
                return new CMP(foper, fsigned, fsz);
            }
          case _con_FCMP:
            {
                auto foper = read_fcmpop(is);
                auto fsz = asdl::read_int(is);
                return new FCMP(foper, fsz);
            }
          case _con_FSGN:
            {
                auto f0 = asdl::read_int(is);
                return new FSGN(f0);
            }
          case _con_PEQL:
            return new PEQL;
          case _con_PNEQ:
            return new PNEQ;
          case _con_STRNEQ:
            {
                auto f0 = asdl::read_int(is);
                return new STRNEQ(f0);
            }
        }
    }
    branch::~branch () { }
    CMP::~CMP () { }
    FCMP::~FCMP () { }
    FSGN::~FSGN () { }
    PEQL::~PEQL () { }
    PNEQ::~PNEQ () { }
    STRNEQ::~STRNEQ () { }
} // namespace CFG_Prim
namespace CFG {
    ty * ty::read (asdl::instream & is)
    {
        _tag_t tag = static_cast<_tag_t>(asdl::read_tag8(is));
        switch (tag) {
          case _con_NUMt:
            {
                auto f0 = asdl::read_int(is);
                return new NUMt(f0);
            }
          case _con_FLTt:
            {
                auto f0 = asdl::read_int(is);
                return new FLTt(f0);
            }
          case _con_PTRt:
            return new PTRt;
          case _con_FUNt:
            return new FUNt;
          case _con_CNTt:
            return new CNTt;
        }
    }
    ty::~ty () { }
    NUMt::~NUMt () { }
    FLTt::~FLTt () { }
    PTRt::~PTRt () { }
    FUNt::~FUNt () { }
    CNTt::~CNTt () { }
    // ty_seq pickler suppressed
    std::vector<ty *> read_ty_seq (asdl::instream & is)
    {
        return asdl::read_seq<ty>(is);
    }
    // pickler suppressed for calling_conv
    calling_conv read_calling_conv (asdl::instream & is)
    {
        return static_cast<calling_conv>(asdl::read_tag8(is));
    }
    exp * exp::read (asdl::instream & is)
    {
        _tag_t tag = static_cast<_tag_t>(asdl::read_tag8(is));
        switch (tag) {
          case _con_VAR:
            {
                auto f0 = LambdaVar::read_lvar(is);
                return new VAR(f0);
            }
          case _con_LABEL:
            {
                auto f0 = LambdaVar::read_lvar(is);
                return new LABEL(f0);
            }
          case _con_NUM:
            {
                auto fiv = asdl::read_integer(is);
                auto fsigned = asdl::read_bool(is);
                auto fsz = asdl::read_int(is);
                return new NUM(fiv, fsigned, fsz);
            }
          case _con_LOOKER:
            {
                auto f0 = CFG_Prim::looker::read_looker(is);
                auto f1 = read_exp_seq(is);
                return new LOOKER(f0, f1);
            }
          case _con_PURE:
            {
                auto f0 = CFG_Prim::pure::read_pure(is);
                auto f1 = read_exp_seq(is);
                return new PURE(f0, f1);
            }
          case _con_SELECT:
            {
                auto f0 = asdl::read_int(is);
                auto f1 = exp::read_exp(is);
                return new SELECT(f0, f1);
            }
          case _con_OFFSET:
            {
                auto f0 = asdl::read_int(is);
                auto f1 = exp::read_exp(is);
                return new OFFSET(f0, f1);
            }
        }
    }
    exp::~exp () { }
    VAR::~VAR () { }
    LABEL::~LABEL () { }
    NUM::~NUM () { }
    LOOKER::~LOOKER ()
    {
        delete this->_v0;
    }
    PURE::~PURE ()
    {
        delete this->_v0;
    }
    SELECT::~SELECT ()
    {
        delete this->_v1;
    }
    OFFSET::~OFFSET ()
    {
        delete this->_v1;
    }
    // exp_seq pickler suppressed
    std::vector<exp *> read_exp_seq (asdl::instream & is)
    {
        return asdl::read_seq<exp>(is);
    }
    param * param::read (asdl::instream & is)
    {
        auto f0 = LambdaVar::read_lvar(is);
        auto f1 = ty::read_ty(is);
        return new param(f0, f1);
    }
    param::~param ()
    {
        delete this->_v1;
    }
    // param_seq pickler suppressed
    std::vector<param *> read_param_seq (asdl::instream & is)
    {
        return asdl::read_seq<param>(is);
    }
    // pickler suppressed for probability
    probability read_probability (asdl::instream & is)
    {
        auto v = asdl::read_int(is);
        return v;
    }
    stm * stm::read (asdl::instream & is)
    {
        _tag_t tag = static_cast<_tag_t>(asdl::read_tag8(is));
        switch (tag) {
          case _con_LET:
            {
                auto f0 = exp::read_exp(is);
                auto f1 = param::read_param(is);
                auto f2 = stm::read_stm(is);
                return new LET(f0, f1, f2);
            }
          case _con_ALLOC:
            {
                auto f0 = CFG_Prim::alloc::read_alloc(is);
                auto f1 = read_exp_seq(is);
                auto f2 = LambdaVar::read_lvar(is);
                auto f3 = stm::read_stm(is);
                return new ALLOC(f0, f1, f2, f3);
            }
          case _con_APPLY:
            {
                auto f0 = read_exp_seq(is);
                auto f1 = read_ty_seq(is);
                return new APPLY(f0, f1);
            }
          case _con_THROW:
            {
                auto f0 = read_exp_seq(is);
                auto f1 = read_ty_seq(is);
                return new THROW(f0, f1);
            }
          case _con_GOTO:
            {
                auto f0 = read_calling_conv(is);
                auto f1 = LambdaVar::read_lvar(is);
                auto f2 = read_exp_seq(is);
                auto f3 = read_ty_seq(is);
                return new GOTO(f0, f1, f2, f3);
            }
          case _con_SWITCH:
            {
                auto f0 = exp::read_exp(is);
                auto f1 = read_stm_seq(is);
                return new SWITCH(f0, f1);
            }
          case _con_BRANCH:
            {
                auto f0 = CFG_Prim::branch::read_branch(is);
                auto f1 = read_exp_seq(is);
                auto f2 = read_probability(is);
                auto f3 = stm::read_stm(is);
                auto f4 = stm::read_stm(is);
                return new BRANCH(f0, f1, f2, f3, f4);
            }
          case _con_ARITH:
            {
                auto f0 = CFG_Prim::arith::read_arith(is);
                auto f1 = read_exp_seq(is);
                auto f2 = param::read_param(is);
                auto f3 = stm::read_stm(is);
                return new ARITH(f0, f1, f2, f3);
            }
          case _con_SETTER:
            {
                auto f0 = CFG_Prim::setter::read_setter(is);
                auto f1 = read_exp_seq(is);
                auto f2 = stm::read_stm(is);
                return new SETTER(f0, f1, f2);
            }
          case _con_RCC:
            {
                auto freentrant = asdl::read_bool(is);
                auto flinkage = asdl::read_string(is);
                auto fproto = CTypes::c_proto::read_c_proto(is);
                auto fargs = read_exp_seq(is);
                auto fresults = read_param_seq(is);
                auto flive = read_param_seq(is);
                auto fk = stm::read_stm(is);
                return new RCC(freentrant, flinkage, fproto, fargs, fresults, flive, fk);
            }
        }
    }
    stm::~stm () { }
    LET::~LET ()
    {
        delete this->_v0;
        delete this->_v1;
        delete this->_v2;
    }
    ALLOC::~ALLOC ()
    {
        delete this->_v0;
        delete this->_v3;
    }
    APPLY::~APPLY () { }
    THROW::~THROW () { }
    GOTO::~GOTO () { }
    SWITCH::~SWITCH ()
    {
        delete this->_v0;
    }
    BRANCH::~BRANCH ()
    {
        delete this->_v0;
        delete this->_v3;
        delete this->_v4;
    }
    ARITH::~ARITH ()
    {
        delete this->_v0;
        delete this->_v2;
        delete this->_v3;
    }
    SETTER::~SETTER ()
    {
        delete this->_v0;
        delete this->_v2;
    }
    RCC::~RCC ()
    {
        delete this->_v_proto;
        delete this->_v_k;
    }
    // stm_seq pickler suppressed
    std::vector<stm *> read_stm_seq (asdl::instream & is)
    {
        return asdl::read_seq<stm>(is);
    }
    // pickler suppressed for frag_kind
    frag_kind read_frag_kind (asdl::instream & is)
    {
        return static_cast<frag_kind>(asdl::read_tag8(is));
    }
    entry * entry::read (asdl::instream & is)
    {
        auto f0 = read_calling_conv(is);
        auto f1 = LambdaVar::read_lvar(is);
        auto f2 = read_param_seq(is);
        auto f3 = stm::read_stm(is);
        return new entry(f0, f1, f2, f3);
    }
    entry::~entry ()
    {
        delete this->_v3;
    }
    frag * frag::read (asdl::instream & is)
    {
        auto f0 = read_frag_kind(is);
        auto f1 = LambdaVar::read_lvar(is);
        auto f2 = read_param_seq(is);
        auto f3 = stm::read_stm(is);
        return new frag(f0, f1, f2, f3);
    }
    frag::~frag ()
    {
        delete this->_v3;
    }
    // frag_seq pickler suppressed
    std::vector<frag *> read_frag_seq (asdl::instream & is)
    {
        return asdl::read_seq<frag>(is);
    }
    attrs * attrs::read (asdl::instream & is)
    {
        auto falignHP = asdl::read_int(is);
        auto fneedsBasePtr = asdl::read_bool(is);
        auto fhasRCC = asdl::read_bool(is);
        return new attrs(falignHP, fneedsBasePtr, fhasRCC);
    }
    attrs::~attrs () { }
    cluster * cluster::read (asdl::instream & is)
    {
        auto f0 = attrs::read_attrs(is);
        auto f1 = entry::read_entry(is);
        auto f2 = read_frag_seq(is);
        return new cluster(f0, f1, f2);
    }
    cluster::~cluster ()
    {
        delete this->_v0;
        delete this->_v1;
    }
} // namespace CFG
