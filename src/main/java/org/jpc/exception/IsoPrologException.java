package org.jpc.exception;

import org.jpc.term.Compound;
import org.jpc.term.Term;

/**
 * An exception in the Prolog side having the format "error(FormalDescription, Context)"
 * @author sergioc
 *
 */
public class IsoPrologException extends PrologException {

	public IsoPrologException(Term exceptionTerm) {
		super(exceptionTerm);
	}
	
	public IsoPrologException(Term exceptionTerm, Term goal) {
		super(exceptionTerm, goal);
	}
	
	public Term getFormalDescriptionTerm() {
		Compound error = (Compound) getExceptionTerm();
		return error.arg(1);
	}
	
	public Term getContextTerm() {
		Compound error = (Compound) getExceptionTerm();
		return error.arg(2);
	}
	
	@Override
	public String getMessage() {
		StringBuilder sb = new StringBuilder(super.getMessage());
		sb.append("Formal description term: " + getFormalDescriptionTerm());
		Term contextTerm = getContextTerm();
		sb.append(". Context term: " + contextTerm);
		sb.append(". ");
		return sb.toString();
	}
	
}
