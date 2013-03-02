package org.jpc.exception;

import org.jpc.term.Term;

/**
 * An exception in the prolog side
 * @author sergioc
 *
 */
public class PrologException extends RuntimeException {

	protected Term goal;
	protected Term exceptionTerm;
	protected String message;
	
	public PrologException(Term goal, Term exceptionTerm) {
		this.goal = goal;
		this.exceptionTerm = exceptionTerm;
	}
	
	public PrologException(Term goal, Term exceptionTerm, String message) {
		this.goal = goal;
		this.exceptionTerm = exceptionTerm;
		this.message = message;
	}

	public Term getGoal() {
		return goal;
	}

	public Term getExceptionTerm() {
		return exceptionTerm;
	}
	
	@Override
	public String getMessage() {
		if(message != null)
			return message;
		else
			return super.getMessage();
	}
	
}
