package org.jpc.term.compiler;

import java.util.ArrayList;
import java.util.Collection;

import org.jpc.term.Term;

/**
 * Internal class.
 * Represents a (term) binding currently bound to one or more variable cells.
 * @author sergioc
 *
 */
public class Register {

	private Term binding; //the bound binding.
	private final Collection<VarCell> cells; //a collection of var cells currently pointing to this register (hence to its current term binding).
	
	/**
	 * 
	 * @param binding the initial term binding associated to this register.
	 */
	public Register(Term binding) {
		this.binding = binding;
		cells = new ArrayList<>();
	}

	public Term getBinding() {
		return binding;
	}

	public void setBinding(Term binding) {
		this.binding = binding;
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

