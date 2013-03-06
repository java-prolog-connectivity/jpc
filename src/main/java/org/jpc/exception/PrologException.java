package org.jpc.exception;

import org.jpc.term.Term;

/**
 * An exception in the Prolog side
 * @author sergioc
 *
 */
public class PrologException extends RuntimeException {

	protected Term exceptionTerm; //the exception term thrown from the Prolog side
	protected Term goal; //the original query that provoked the exception
	

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
