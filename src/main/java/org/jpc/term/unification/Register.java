package org.jpc.term.unification;

import java.util.ArrayList;
import java.util.Collection;

import org.jpc.term.Term;

/**
 * Internal class.
 * Represents a (term) value currently bound to one or more variable cells.
 * @author sergioc
 *
 */
public class Register {

	private Term value; //the bound value.
	private final Collection<VarCell> cells; //a collection of var cells currently pointing to this register (hence to its current term value).
	
	/**
	 * 
	 * @param value the initial term value associated to this register.
	 */
	public Register(Term value) {
		this.value = value;
		cells = new ArrayList<>();
	}

	public Term getValue() {
		return value;
	}

	public void setValue(Term value) {
		this.value = value;
	}
	
	public void addAll(Collection<VarCell> varCells) {
		for(VarCell varCell : varCells) {
			add(varCell);
		}
	}
	
	public void add(VarCell varCell) {
		cells.add(varCell);
		varCell.setRegister(this);
	}
	
	public Collection<VarCell> getCells() {
		return cells;
	}
	
	public void becomes(Register register) {
		register.addAll(getCells());
		cells.clear();
	}
}

