package org.jpc.exception;

import org.jpc.term.Term;

/**
 * An exception in the prolog side
 * @author sergioc
 *
 */
public class PrologException extends RuntimeException {

	protected Term exceptionTerm;
	protected Term goal;
	

	public PrologException(Term exceptionTerm) {
		this.exceptionTerm = exceptionTerm;
	}
	
	public PrologException(Term exceptionTerm, Term goal) {
		this.exceptionTerm = exceptionTerm;
		this.goal = goal;
	}

	public Term getExceptionTerm() {
		return exceptionTerm;
	}
	
	public Term getGoal() {
		return goal;
	}

	@Override
	public String getMessage() {
		StringBuilder sb = new StringBuilder("Exception Term: " + exceptionTerm);
		if(goal != null)
			sb.append(" while executing Goal: " + goal);
		sb.append(". ");
		return sb.toString();
	}
	
}
