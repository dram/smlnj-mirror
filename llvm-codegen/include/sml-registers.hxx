/// \file sml-registers.hxx
///
/// \copyright 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
/// All rights reserved.
///
/// \brief This file defines the `reg_state` class, which encapsulates the
///        state of the "SML" registers (e.g., allocation pointer, limit pointer, ...).
///
/// \author John Reppy
///

#ifndef _SML_REGISTERS_HXX_
#define _SML_REGISTERS_HXX_

#include "llvm/IR/Value.h"

#include <string>
#include <vector>

// the SML Machine "registers"; the number of miscellaneous registers is
// target dependent, but we assume that no targets support more than 20 misc. regs
//
enum class sml_reg_id {
	ALLOC_PTR = 0,		// allocation pointer
	LIMIT_PTR,		// heap-limit pointer
	STORE_PTR,		// points to list of store records
	EXN_HNDLR,		// exception handler
	VAR_PTR,		// var_ptr register
	GC_LINK,
	BASE_PTR,		// points to base address of module (optional)
	STD_CONT,
	STD_ARG,
	STD_LINK,
	STD_CLOS,
	MISC_REG0,		// first miscellaneous register
	MISC_REG1,
	MISC_REG2,
	MISC_REG3,
	MISC_REG4,
	MISC_REG5,
	MISC_REG6,
	MISC_REG7,
	MISC_REG8,
	MISC_REG9,
	MISC_REG10,
	MISC_REG11,
	MISC_REG12,
	MISC_REG13,
	MISC_REG14,
	MISC_REG15,
	MISC_REG16,
	MISC_REG17,
	MISC_REG18,
	MISC_REG19,
	NUM_REGS
};

const int NUM_REGS = static_cast<int>(sml_reg_id::NUM_REGS);

class reg_info {
  public:

  // functions for creating registers
    static reg_info *createReg (int idx) { return new reg_info (idx, 0); }
    static reg_info *createStkReg (int offset) { return new reg_info (-1, offset); }

    bool isMachineReg () const { return (this->_idx >= 0); }
    bool isMemReg () const { return (this->_idx < 0); }

    std::string const &name () const { return this->_name; }

  private:
    int		_idx;		// index of hardware register assigned to this register.
				// This value is the parameter index in the JWA calling
				// convention.  It will be -1 for stack allocated registers
    int		_offset;	// For stack allocated registers, this is the offset from
				// the stack pointer to where the register is allocated in
				// the frame
    std::string _name;		// the register's name

    reg_info (int idx, int off) : _idx(idx), _offset(off) { }

};

// information about the arguments for a standard function or continuation call
//
struct argument_info {
    std::vector<sml_reg_id>	formals;	// the registers used to pass the arguments
    std::vector<llvm::Type *>	tys;		// the LLVM types for the parameters
};

// information about the "SML" registers for a given target
//
class sml_registers {
  public:

  // setup the register information for the specified target architecture
  //
    sml_registers (struct target_info const *target);

    int numRegs () const { return this->_nRegs; }

    reg_info const *info (sml_reg_id id) const { return this->_info[static_cast<int>(id)]; }

    int numSpecialRegs () const { return this->_nSpecialRegs; }

    sml_reg_id specialId (int idx) const { return this->_specialRegs[idx]; }
    reg_info const *special (int idx) const { return this->info(this->_specialRegs[idx]); }

  private:
    bool		_hasBaseReg;		// true if target needs the base register to
						// compute code-address values
    int			_nRegs;			// number of registers supported by target
    int			_nSpecialRegs;		// the number of special registers that are
						// hardware supported.
    reg_info *		_info[NUM_REGS];	// information about the registers;
						// _info[BASE_PTR] will be null if _hasBaseReg
						// is false.  Otherwise, _info[i] will be
						// non-null for 0 <= i < _nRegs.
    sml_reg_id *	_specialRegs;
};

/* FIXME: I think that we only need to track the special registers here, since the
 * other registers are named by lambda variables, which are tracked by the cluster's
 * lvar-to-value map.
 */
class reg_state {
  public:

    reg_state () { }
    explicit reg_state (sml_registers const & info);
    ~reg_state () { }

  // get the LLVM value that represents the specified SML register
    llvm::Value *get (sml_reg_id r) const
    {
	return this->_val[static_cast<int>(r)];
    }

  // assign a value to an SML register
    void set (sml_reg_id r, llvm::Value *v)
    {
	this->_val[static_cast<int>(r)] = v;
    }

    void copyFrom (reg_state const & cache);

  private:
    int			_nRegs;
    llvm::Value *	_val[NUM_REGS];		// mapping from registers IDs to their current
						// representation as an LLVM value.
};

sml_registers *InitRegInfo (std::string const &target);

#endif // !_SML_REGISTERS_HXX_
