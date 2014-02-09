package org.jpc.term.compiled;

import org.jpc.JpcException;
import org.jpc.term.AbstractVar;
import org.jpc.term.Term;


public class BindableVar extends AbstractVar {

	private final VarCell varCell;
	private final AbstractVar var;
	
	BindableVar(AbstractVar var) {
		this.var = var;
		varCell = new VarCell(var);
	}
	
	public Term getBinding() {
		return varCell.getBinding();
	}
	
	@Override
	public boolean termEquals(Term term) {
		if (this == term)
			return true;
		if (term == null)
			return false;
		if (getClass() != term.getClass())
			return false;
		BindableVar other = (BindableVar) term;
		return var.termEquals(other.var);
	}
	
	@Override
	public Term compile(int clauseId, CompilationContext context) {
		throw new UnsupportedOperationException();
	}

	@Override
	public Term prepareForQuery(CompilationContext context) {
		throw new UnsupportedOperationException();
	}

	@Override
	public Term prepareForFrame(CompilationContext context) {
		throw new UnsupportedOperationException();
	}

	@Override
	public boolean isAnonymous() {
		return var.isAnonymous();
	}

	@Override
	public String getName() {
		return var.getName();
	}
	
	public VarCell getVarCell() {
		return varCell;
	}
	
	public AbstractVar getVar() {
		return var;
	}
	
	private boolean hasAnonymousBinding(Term term) {
		return term instanceof BindableVar && ((BindableVar)term).getBinding() instanceof AbstractVar && ((AbstractVar)((BindableVar)term).getBinding()).isAnonymous();
	}
	
	@Override
	public void doUnification(Term term) {
		if( !(hasAnonymousBinding(this) || hasAnonymousBinding(term)) ) {
			VarCell thisVarCell = getVarCell();
			if(term instanceof AbstractVar) {
				if(!(term instanceof BindableVar))
					throw new JpcException("Attempt to unify non-compiled term.");
				BindableVar thatVar = (BindableVar) term;
				VarCell thatVarCell = thatVar.getVarCell();
				if(thisVarCell.getRegister() != thatVarCell.getRegister()) {
					Term thatVarBoundTerm = thatVarCell.getBinding();
					if(!(thatVarBoundTerm instanceof AbstractVar))
						unifyCell(thisVarCell, thatVarBoundTerm);
					thatVarCell.getRegister().becomes(thisVarCell.getRegister());
				}
			} else {
				unifyCell(thisVarCell, term);
			}
		}
	}
	
	private void unifyCell(VarCell varCell, Term term) {
		Term oldTerm = varCell.getBinding();
		if(oldTerm instanceof AbstractVar) {
			varCell.setBinding(term);
		} else {
			oldTerm.doUnification(term);
		}
	}
	
}
