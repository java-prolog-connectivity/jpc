package org.jpc.term.unification;

import org.jpc.term.Term;

/**
 * Internal class.
 * Encapsulates a register reference associated to a given term variable.
 * @author sergioc
 *
 */
public class VarCell {

	private Register register;

	public VarCell(Term value) {
		this(new Register(value));
	}
	
	public VarCell(Register register) {
		register.add(this);
	}
	
	public Register getRegister() {
		return register;
	}

	public void setRegister(Register register) {
		this.register = register;
	}
	
	public Term getValue() {
		return getRegister().getValue();
	}
	
	public void setValue(Term term) {
		getRegister().setValue(term);
	}

}
