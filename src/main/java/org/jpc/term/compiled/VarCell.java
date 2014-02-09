package org.jpc.term.compiled;

import org.jpc.term.Term;

/**
 * Internal class.
 * Encapsulates a register reference associated to a given term variable.
 * @author sergioc
 *
 */
public class VarCell {

	private Register register;

	public VarCell(Term binding) {
		this(new Register(binding));
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
	
	public Term getBinding() {
		return getRegister().getBinding();
	}
	
	public void setBinding(Term binding) {
		getRegister().setBinding(binding);
	}

}
