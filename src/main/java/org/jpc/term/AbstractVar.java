package org.jpc.term;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.jpc.engine.prolog.OperatorsContext;
import org.jpc.salt.TermContentHandler;
import org.jpc.term.unification.VarCell;
import org.jpc.term.visitor.TermVisitor;

import com.google.common.base.Function;

public abstract class AbstractVar extends Term {

	/**
	 * @return the variables names present in the term
	 */
	public static List<String> getVariableNames(List<AbstractVar> vars) {
		List<String> names = new ArrayList<>();
		for(AbstractVar var : vars) {
			names.add(var.getName());
		}
		return names;
	}
	
	@Override
	public boolean hasFunctor(Functor functor) {
		return functor.getArity() == 0 && termEquals(functor.getName());
	}

	@Override
	public void accept(TermVisitor termVisitor) {
		termVisitor.visitVariable(this);
	}

	@Override
	protected void basicRead(TermContentHandler contentHandler, Function<Term, Term> termExpander) {
		contentHandler.startVariable(getName());
		
	}

	@Override
	public boolean isGround() {
		return false;
	}
	
	@Override
	protected void unifyVars(Term term, Map<AbstractVar, VarCell> context) {
		if(!(isAnonymous() || 
				(term instanceof AbstractVar && ((AbstractVar)term).isAnonymous()))) {
			VarCell thisVarCell = context.get(this);
			if(thisVarCell == null) {
				thisVarCell = new VarCell(this);
				context.put(this, thisVarCell);
			}
			if(term instanceof AbstractVar) {
				VarCell thatVarCell = context.get(term);
				if(thatVarCell == null) {
					thatVarCell = thisVarCell;
					context.put((AbstractVar)term, thatVarCell);
				} 
				if(thisVarCell != thatVarCell) {
					Term thatVarBoundTerm = thatVarCell.getValue();
					unifyCell(thisVarCell, thatVarBoundTerm, context);
					thatVarCell.getRegister().becomes(thisVarCell.getRegister());
				}
			} else {
				unifyCell(thisVarCell, term, context);
			}
		}
	}
	
	private void unifyCell(VarCell varCell, Term term, Map<AbstractVar, VarCell> context) {
		Term oldTerm = varCell.getValue();
		if(oldTerm instanceof AbstractVar) {
			if(!(term instanceof AbstractVar))
				varCell.setValue(term);
		} else {
			oldTerm.unifyVars(term, context);
		}
	}
	
	@Override
	public String toString(OperatorsContext operatorsContext) {
		return toString();
	}
	
	/**
	 * Returns a Prolog source text representation of this Variable
	 * 
	 * @return  a Prolog source text representation of this Variable
	 */
	@Override
	public String toString() {
		return getName();
	}
	
	@Override
	public String toEscapedString() {
		return toString();
	}
	
	/**
	 * A Variable is equal to another if their names are the same and they are not anonymous.
	 * 
	 * @param   obj  The Object to compare.
	 * @return  true if the Object is a Variable and the above condition apply.
	 */
	@Override
	public boolean equals(Object obj) {
		try {
			return !isAnonymous() && termEquals((Term) obj);
		} catch(ClassCastException e) {
			return false;
		}
	}
	
	public abstract boolean isAnonymous();
	
	public abstract String getName();
	
}
