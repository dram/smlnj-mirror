// cps.cxx
//
// Generated from cps.asdl by asdlgen.
//

#include "cps.hxx"

namespace CTypes {
    c_type * c_type::read (asdl::instream & is)
    {
        _tag_t tag = static_cast<_tag_t>(asdl::rd_tag8(is));
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
                auto f0 = c_type::read(is);
                auto f1 = asdl::rd_int(is);
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
    // pickler suppressed for c_int
    c_int read_c_int (asdl::instream & is)
    {
        return static_cast<c_int>(asdl::rd_tag8(is));
    }
    // pickler suppressed for calling_convention
    calling_convention read_calling_convention (asdl::instream & is)
    {
        auto v = asdl::rd_string(is);
        return v;
    }
    c_proto * c_proto::read (asdl::instream & is)
    {
        auto fconv = read_calling_convention(is);
        auto fretTy = c_type::read(is);
        auto fparamTys = read_c_type_seq(is);
        return new c_proto(fconv, fretTy, fparamTys);
    }
    c_proto::~c_proto ()
    {
        delete this->_v_retTy;
    }
} // namespace CTypes
namespace CPS_Type {
    // pickler suppressed for record_kind
    record_kind read_record_kind (asdl::instream & is)
    {
        return static_cast<record_kind>(asdl::rd_tag8(is));
    }
    pkind * pkind::read (asdl::instream & is)
    {
        _tag_t tag = static_cast<_tag_t>(asdl::rd_tag8(is));
        switch (tag) {
          case _con_VPT:
            return new VPT;
          case _con_RPT:
            {
                auto f0 = asdl::rd_int(is);
                return new RPT(f0);
            }
          case _con_FPT:
            {
                auto f0 = asdl::rd_int(is);
                return new FPT(f0);
            }
        }
    }
    pkind::~pkind () { }
    VPT::~VPT () { }
    RPT::~RPT () { }
    FPT::~FPT () { }
    intty * intty::read (asdl::instream & is)
    {
        auto fsz = asdl::rd_int(is);
        auto ftag = asdl::rd_bool(is);
        return new intty(fsz, ftag);
    }
    intty::~intty () { }
    cty * cty::read (asdl::instream & is)
    {
        _tag_t tag = static_cast<_tag_t>(asdl::rd_tag8(is));
        switch (tag) {
          case _con_NUMt:
            {
                auto f0 = intty::read(is);
                return new NUMt(f0);
            }
          case _con_PTRt:
            {
                auto f0 = pkind::read(is);
                return new PTRt(f0);
            }
          case _con_FUNt:
            return new FUNt;
          case _con_FLTt:
            {
                auto f0 = asdl::rd_int(is);
                return new FLTt(f0);
            }
          case _con_CNTt:
            return new CNTt;
        }
    }
    cty::~cty () { }
    NUMt::~NUMt ()
    {
        delete this->_v0;
    }
    PTRt::~PTRt ()
    {
        delete this->_v0;
    }
    FUNt::~FUNt () { }
    FLTt::~FLTt () { }
    CNTt::~CNTt () { }
} // namespace CPS_Type
namespace CPS_IntConst {
    t * t::read (asdl::instream & is)
    {
        auto fival = asdl::rd_integer(is);
        auto fty = CPS_Type::intty::read(is);
        return new t(fival, fty);
    }
    t::~t ()
    {
        delete this->_v_ty;
    }
} // namespace CPS_IntConst
namespace CPS_RealConst {
    t * t::read (asdl::instream & is)
    {
        auto frval = RealLit::read_t(is);
        auto fty = asdl::rd_int(is);
        return new t(frval, fty);
    }
    t::~t () { }
} // namespace CPS_RealConst
namespace CPS_Prim {
    numkind * numkind::read (asdl::instream & is)
    {
        _tag_t tag = static_cast<_tag_t>(asdl::rd_tag8(is));
        auto f0 = asdl::rd_int(is);
        switch (tag) {
          case _con_INT:
            return new INT(f0);
          case _con_UINT:
            return new UINT(f0);
          case _con_FLT:
            return new FLT(f0);
        }
    }
    numkind::~numkind () { }
    INT::~INT () { }
    UINT::~UINT () { }
    FLT::~FLT () { }
    // pickler suppressed for arithop
    arithop read_arithop (asdl::instream & is)
    {
        return static_cast<arithop>(asdl::rd_tag8(is));
    }
    // pickler suppressed for pureop
    pureop read_pureop (asdl::instream & is)
    {
        return static_cast<pureop>(asdl::rd_tag8(is));
    }
    // pickler suppressed for cmpop
    cmpop read_cmpop (asdl::instream & is)
    {
        return static_cast<cmpop>(asdl::rd_tag8(is));
    }
    // pickler suppressed for fcmpop
    fcmpop read_fcmpop (asdl::instream & is)
    {
        return static_cast<fcmpop>(asdl::rd_tag8(is));
    }
    branch * branch::read (asdl::instream & is)
    {
        _tag_t tag = static_cast<_tag_t>(asdl::rd_tag8(is));
        switch (tag) {
          case _con_CMP:
            {
                auto foper = read_cmpop(is);
                auto fsigned = asdl::rd_bool(is);
                return new CMP(foper, fsigned);
            }
          case _con_FCMP:
            {
                auto foper = read_fcmpop(is);
                auto fsize = asdl::rd_int(is);
                return new FCMP(foper, fsize);
            }
          case _con_FSGN:
            {
                auto f0 = asdl::rd_int(is);
                return new FSGN(f0);
            }
          case _con_BOXED:
            return new BOXED;
          case _con_UNBOXED:
            return new UNBOXED;
          case _con_PEQL:
            return new PEQL;
          case _con_PNEQ:
            return new PNEQ;
          case _con_STREQL:
            return new STREQL;
          case _con_STRNEQ:
            return new STRNEQ;
        }
    }
    branch::~branch () { }
    CMP::~CMP () { }
    FCMP::~FCMP () { }
    FSGN::~FSGN () { }
    BOXED::~BOXED () { }
    UNBOXED::~UNBOXED () { }
    PEQL::~PEQL () { }
    PNEQ::~PNEQ () { }
    STREQL::~STREQL () { }
    STRNEQ::~STRNEQ () { }
    setter * setter::read (asdl::instream & is)
    {
        _tag_t tag = static_cast<_tag_t>(asdl::rd_tag8(is));
        switch (tag) {
          case _con_NUMUPDATE:
            {
                auto fkind = numkind::read(is);
                return new NUMUPDATE(fkind);
            }
          case _con_UNBOXEDUPDATE:
            return new UNBOXEDUPDATE;
          case _con_UPDATE:
            return new UPDATE;
          case _con_UNBOXEDASSIGN:
            return new UNBOXEDASSIGN;
          case _con_ASSIGN:
            return new ASSIGN;
          case _con_SETHDLR:
            return new SETHDLR;
          case _con_SETVAR:
            return new SETVAR;
          case _con_SETSPECIAL:
            return new SETSPECIAL;
          case _con_FREE:
            return new FREE;
          case _con_ACCLINK:
            return new ACCLINK;
          case _con_SETPSEUDO:
            return new SETPSEUDO;
          case _con_SETMARK:
            return new SETMARK;
          case _con_RAWSTORE:
            {
                auto fkind = numkind::read(is);
                return new RAWSTORE(fkind);
            }
          case _con_RAWUPDATE:
            {
                auto f0 = CPS_Type::cty::read(is);
                return new RAWUPDATE(f0);
            }
        }
    }
    setter::~setter () { }
    NUMUPDATE::~NUMUPDATE ()
    {
        delete this->_v_kind;
    }
    UNBOXEDUPDATE::~UNBOXEDUPDATE () { }
    UPDATE::~UPDATE () { }
    UNBOXEDASSIGN::~UNBOXEDASSIGN () { }
    ASSIGN::~ASSIGN () { }
    SETHDLR::~SETHDLR () { }
    SETVAR::~SETVAR () { }
    SETSPECIAL::~SETSPECIAL () { }
    FREE::~FREE () { }
    ACCLINK::~ACCLINK () { }
    SETPSEUDO::~SETPSEUDO () { }
    SETMARK::~SETMARK () { }
    RAWSTORE::~RAWSTORE ()
    {
        delete this->_v_kind;
    }
    RAWUPDATE::~RAWUPDATE ()
    {
        delete this->_v0;
    }
    looker * looker::read (asdl::instream & is)
    {
        _tag_t tag = static_cast<_tag_t>(asdl::rd_tag8(is));
        switch (tag) {
          case _con_DEREF:
            return new DEREF;
          case _con_SUBSCRIPT:
            return new SUBSCRIPT;
          case _con_NUMSUBSCRIPT:
            {
                auto fkind = numkind::read(is);
                return new NUMSUBSCRIPT(fkind);
            }
          case _con_GETSPECIAL:
            return new GETSPECIAL;
          case _con_GETHDLR:
            return new GETHDLR;
          case _con_GETVAR:
            return new GETVAR;
          case _con_GETPSEUDO:
            return new GETPSEUDO;
          case _con_RAWLOAD:
            {
                auto fkind = numkind::read(is);
                return new RAWLOAD(fkind);
            }
        }
    }
    looker::~looker () { }
    DEREF::~DEREF () { }
    SUBSCRIPT::~SUBSCRIPT () { }
    NUMSUBSCRIPT::~NUMSUBSCRIPT ()
    {
        delete this->_v_kind;
    }
    GETSPECIAL::~GETSPECIAL () { }
    GETHDLR::~GETHDLR () { }
    GETVAR::~GETVAR () { }
    GETPSEUDO::~GETPSEUDO () { }
    RAWLOAD::~RAWLOAD ()
    {
        delete this->_v_kind;
    }
    arith * arith::read (asdl::instream & is)
    {
        _tag_t tag = static_cast<_tag_t>(asdl::rd_tag8(is));
        switch (tag) {
          case _con_ARITH:
            {
                auto foper = read_arithop(is);
                auto fkind = numkind::read(is);
                return new ARITH(foper, fkind);
            }
          case _con_TEST:
            {
                auto ffrom = asdl::rd_int(is);
                auto fto = asdl::rd_int(is);
                return new TEST(ffrom, fto);
            }
          case _con_TESTU:
            {
                auto ffrom = asdl::rd_int(is);
                auto fto = asdl::rd_int(is);
                return new TESTU(ffrom, fto);
            }
          case _con_TEST_INF:
            {
                auto f0 = asdl::rd_int(is);
                return new TEST_INF(f0);
            }
          case _con_REAL_TO_INT:
            {
                auto ffloor = asdl::rd_bool(is);
                auto ffrom = asdl::rd_int(is);
                auto fto = asdl::rd_int(is);
                return new REAL_TO_INT(ffloor, ffrom, fto);
            }
        }
    }
    arith::~arith () { }
    ARITH::~ARITH ()
    {
        delete this->_v_kind;
    }
    TEST::~TEST () { }
    TESTU::~TESTU () { }
    TEST_INF::~TEST_INF () { }
    REAL_TO_INT::~REAL_TO_INT () { }
    pure * pure::read (asdl::instream & is)
    {
        _tag_t tag = static_cast<_tag_t>(asdl::rd_tag8(is));
        switch (tag) {
          case _con_PURE_ARITH:
            {
                auto foper = read_arithop(is);
                auto fkind = numkind::read(is);
                return new PURE_ARITH(foper, fkind);
            }
          case _con_PURE_NUMSUBSCRIPT:
            {
                auto fkind = numkind::read(is);
                return new PURE_NUMSUBSCRIPT(fkind);
            }
          case _con_LENGTH:
            return new LENGTH;
          case _con_OBJLENGTH:
            return new OBJLENGTH;
          case _con_MAKEREF:
            return new MAKEREF;
          case _con_COPY:
            {
                auto ffrom = asdl::rd_int(is);
                auto fto = asdl::rd_int(is);
                return new COPY(ffrom, fto);
            }
          case _con_EXTEND:
            {
                auto ffrom = asdl::rd_int(is);
                auto fto = asdl::rd_int(is);
                return new EXTEND(ffrom, fto);
            }
          case _con_TRUNC:
            {
                auto ffrom = asdl::rd_int(is);
                auto fto = asdl::rd_int(is);
                return new TRUNC(ffrom, fto);
            }
          case _con_COPY_INF:
            {
                auto f0 = asdl::rd_int(is);
                return new COPY_INF(f0);
            }
          case _con_EXTEND_INF:
            {
                auto f0 = asdl::rd_int(is);
                return new EXTEND_INF(f0);
            }
          case _con_TRUNC_INF:
            {
                auto f0 = asdl::rd_int(is);
                return new TRUNC_INF(f0);
            }
          case _con_INT_TO_REAL:
            {
                auto ffrom = asdl::rd_int(is);
                auto fto = asdl::rd_int(is);
                return new INT_TO_REAL(ffrom, fto);
            }
          case _con_SUBSCRIPTV:
            return new SUBSCRIPTV;
          case _con_GETTAG:
            return new GETTAG;
          case _con_MKSPECIAL:
            return new MKSPECIAL;
          case _con_CAST:
            return new CAST;
          case _con_GETCON:
            return new GETCON;
          case _con_GETEXN:
            return new GETEXN;
          case _con_BOX:
            return new BOX;
          case _con_UNBOX:
            return new UNBOX;
          case _con_WRAP:
            {
                auto f0 = numkind::read(is);
                return new WRAP(f0);
            }
          case _con_UNWRAP:
            {
                auto f0 = numkind::read(is);
                return new UNWRAP(f0);
            }
          case _con_FWRAP:
            return new FWRAP;
          case _con_FUNWRAP:
            return new FUNWRAP;
          case _con_IWRAP:
            return new IWRAP;
          case _con_IUNWRAP:
            return new IUNWRAP;
          case _con_I32WRAP:
            return new I32WRAP;
          case _con_I32UNWRAP:
            return new I32UNWRAP;
          case _con_GETSEQDATA:
            return new GETSEQDATA;
          case _con_RECSUBSCRIPT:
            return new RECSUBSCRIPT;
          case _con_RAW64SUBSCRIPT:
            return new RAW64SUBSCRIPT;
          case _con_NEWARRAY0:
            return new NEWARRAY0;
          case _con_RAWRECORD:
            {
                auto f0 = CPS_Type::read_record_kind_option(is);
                return new RAWRECORD(f0);
            }
        }
    }
    pure::~pure () { }
    PURE_ARITH::~PURE_ARITH ()
    {
        delete this->_v_kind;
    }
    PURE_NUMSUBSCRIPT::~PURE_NUMSUBSCRIPT ()
    {
        delete this->_v_kind;
    }
    LENGTH::~LENGTH () { }
    OBJLENGTH::~OBJLENGTH () { }
    MAKEREF::~MAKEREF () { }
    COPY::~COPY () { }
    EXTEND::~EXTEND () { }
    TRUNC::~TRUNC () { }
    COPY_INF::~COPY_INF () { }
    EXTEND_INF::~EXTEND_INF () { }
    TRUNC_INF::~TRUNC_INF () { }
    INT_TO_REAL::~INT_TO_REAL () { }
    SUBSCRIPTV::~SUBSCRIPTV () { }
    GETTAG::~GETTAG () { }
    MKSPECIAL::~MKSPECIAL () { }
    CAST::~CAST () { }
    GETCON::~GETCON () { }
    GETEXN::~GETEXN () { }
    BOX::~BOX () { }
    UNBOX::~UNBOX () { }
    WRAP::~WRAP ()
    {
        delete this->_v0;
    }
    UNWRAP::~UNWRAP ()
    {
        delete this->_v0;
    }
    FWRAP::~FWRAP () { }
    FUNWRAP::~FUNWRAP () { }
    IWRAP::~IWRAP () { }
    IUNWRAP::~IUNWRAP () { }
    I32WRAP::~I32WRAP () { }
    I32UNWRAP::~I32UNWRAP () { }
    GETSEQDATA::~GETSEQDATA () { }
    RECSUBSCRIPT::~RECSUBSCRIPT () { }
    RAW64SUBSCRIPT::~RAW64SUBSCRIPT () { }
    NEWARRAY0::~NEWARRAY0 () { }
    RAWRECORD::~RAWRECORD () { }
} // namespace CPS_Prim
namespace CPS_Rep {
    value * value::read (asdl::instream & is)
    {
        _tag_t tag = static_cast<_tag_t>(asdl::rd_tag8(is));
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
                auto f0 = CPS_IntConst::t::read(is);
                return new NUM(f0);
            }
          case _con_REAL:
            {
                auto f0 = CPS_RealConst::t::read(is);
                return new REAL(f0);
            }
          case _con_STRING:
            {
                auto f0 = asdl::rd_string(is);
                return new STRING(f0);
            }
          case _con_VOID:
            return new VOID;
        }
    }
    value::~value () { }
    VAR::~VAR () { }
    LABEL::~LABEL () { }
    NUM::~NUM ()
    {
        delete this->_v0;
    }
    REAL::~REAL ()
    {
        delete this->_v0;
    }
    STRING::~STRING () { }
    VOID::~VOID () { }
    accesspath * accesspath::read (asdl::instream & is)
    {
        _tag_t tag = static_cast<_tag_t>(asdl::rd_tag8(is));
        switch (tag) {
          case _con_OFFp:
            {
                auto f0 = asdl::rd_int(is);
                return new OFFp(f0);
            }
          case _con_SELp:
            {
                auto f0 = asdl::rd_int(is);
                auto f1 = accesspath::rd_accesspath(is);
                return new SELp(f0, f1);
            }
        }
    }
    accesspath::~accesspath () { }
    OFFp::~OFFp () { }
    SELp::~SELp ()
    {
        delete this->_v1;
    }
    // pickler suppressed for fun_kind
    fun_kind read_fun_kind (asdl::instream & is)
    {
        return static_cast<fun_kind>(asdl::rd_tag8(is));
    }
    cexp * cexp::read (asdl::instream & is)
    {
        _tag_t tag = static_cast<_tag_t>(asdl::rd_tag8(is));
        switch (tag) {
          case _con_RECORD:
            {
                auto f0 = CPS_Type::read_record_kind(is);
                auto f1 = read_field_seq(is);
                auto f2 = LambdaVar::read_lvar(is);
                auto f3 = cexp::read(is);
                return new RECORD(f0, f1, f2, f3);
            }
          case _con_SELECT:
            {
                auto f0 = asdl::rd_int(is);
                auto f1 = value::read(is);
                auto f2 = LambdaVar::read_lvar(is);
                auto f3 = CPS_Type::cty::read(is);
                auto f4 = cexp::read(is);
                return new SELECT(f0, f1, f2, f3, f4);
            }
          case _con_OFFSET:
            {
                auto f0 = asdl::rd_int(is);
                auto f1 = value::read(is);
                auto f2 = LambdaVar::read_lvar(is);
                auto f3 = cexp::read(is);
                return new OFFSET(f0, f1, f2, f3);
            }
          case _con_APP:
            {
                auto f0 = value::read(is);
                auto f1 = read_value_seq(is);
                return new APP(f0, f1);
            }
          case _con_FIX:
            {
                auto f0 = read_function_seq(is);
                auto f1 = cexp::read(is);
                return new FIX(f0, f1);
            }
          case _con_SWITCH:
            {
                auto f0 = value::read(is);
                auto f1 = LambdaVar::read_lvar(is);
                auto f2 = read_cexp_seq(is);
                return new SWITCH(f0, f1, f2);
            }
          case _con_BRANCH:
            {
                auto f0 = CPS_Prim::branch::read(is);
                auto f1 = read_value_seq(is);
                auto f2 = LambdaVar::read_lvar(is);
                auto f3 = cexp::read(is);
                auto f4 = cexp::read(is);
                return new BRANCH(f0, f1, f2, f3, f4);
            }
          case _con_SETTER:
            {
                auto f0 = CPS_Prim::setter::read(is);
                auto f1 = read_value_seq(is);
                auto f2 = cexp::read(is);
                return new SETTER(f0, f1, f2);
            }
          case _con_LOOKER:
            {
                auto f0 = CPS_Prim::looker::read(is);
                auto f1 = read_value_seq(is);
                auto f2 = LambdaVar::read_lvar(is);
                auto f3 = CPS_Type::cty::read(is);
                auto f4 = cexp::read(is);
                return new LOOKER(f0, f1, f2, f3, f4);
            }
          case _con_ARITH:
            {
                auto f0 = CPS_Prim::arith::read(is);
                auto f1 = read_value_seq(is);
                auto f2 = LambdaVar::read_lvar(is);
                auto f3 = CPS_Type::cty::read(is);
                auto f4 = cexp::read(is);
                return new ARITH(f0, f1, f2, f3, f4);
            }
          case _con_PURE:
            {
                auto f0 = CPS_Prim::pure::read(is);
                auto f1 = read_value_seq(is);
                auto f2 = LambdaVar::read_lvar(is);
                auto f3 = CPS_Type::cty::read(is);
                auto f4 = cexp::read(is);
                return new PURE(f0, f1, f2, f3, f4);
            }
          case _con_RCC:
            {
                auto f0 = read_rcc_kind(is);
                auto f1 = asdl::rd_string(is);
                auto f2 = CTypes::c_proto::read(is);
                auto f3 = read_value_seq(is);
                auto f4 = read_rcc_param_seq(is);
                auto f5 = cexp::read(is);
                return new RCC(f0, f1, f2, f3, f4, f5);
            }
        }
    }
    cexp::~cexp () { }
    RECORD::~RECORD ()
    {
        delete this->_v3;
    }
    SELECT::~SELECT ()
    {
        delete this->_v1;
        delete this->_v3;
        delete this->_v4;
    }
    OFFSET::~OFFSET ()
    {
        delete this->_v1;
        delete this->_v3;
    }
    APP::~APP ()
    {
        delete this->_v0;
    }
    FIX::~FIX ()
    {
        delete this->_v1;
    }
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
    SETTER::~SETTER ()
    {
        delete this->_v0;
        delete this->_v2;
    }
    LOOKER::~LOOKER ()
    {
        delete this->_v0;
        delete this->_v3;
        delete this->_v4;
    }
    ARITH::~ARITH ()
    {
        delete this->_v0;
        delete this->_v3;
        delete this->_v4;
    }
    PURE::~PURE ()
    {
        delete this->_v0;
        delete this->_v3;
        delete this->_v4;
    }
    RCC::~RCC ()
    {
        delete this->_v2;
        delete this->_v5;
    }
    field * field::read (asdl::instream & is)
    {
        auto f0 = value::read(is);
        auto f1 = accesspath::rd_accesspath(is);
        return new field(f0, f1);
    }
    field::~field ()
    {
        delete this->_v0;
        delete this->_v1;
    }
    rcc_param * rcc_param::read (asdl::instream & is)
    {
        auto f0 = LambdaVar::read_lvar(is);
        auto f1 = CPS_Type::cty::read(is);
        return new rcc_param(f0, f1);
    }
    rcc_param::~rcc_param ()
    {
        delete this->_v1;
    }
    // pickler suppressed for rcc_kind
    rcc_kind read_rcc_kind (asdl::instream & is)
    {
        return static_cast<rcc_kind>(asdl::rd_tag8(is));
    }
    function * function::read (asdl::instream & is)
    {
        auto f0 = read_fun_kind(is);
        auto f1 = LambdaVar::read_lvar(is);
        auto f2 = LambdaVar::read_lvar_seq(is);
        auto f3 = CPS_Type::read_cty_seq(is);
        auto f4 = cexp::read(is);
        return new function(f0, f1, f2, f3, f4);
    }
    function::~function ()
    {
        delete this->_v4;
    }
} // namespace CPS_Rep
