package org.jpc.exception.logtalk;

import org.jpc.exception.PrologException;
import org.jpc.term.Compound;
import org.jpc.term.Term;
import org.jpc.term.Variable;

public class LogtalkException extends PrologException {

	public LogtalkException(Term exceptionTerm) {
		super(exceptionTerm);
	}
	
	public LogtalkException(Term exceptionTerm, Term goal) {
		super(exceptionTerm, goal);
	}
	
	public Term getLgtExceptionTerm() {
		Compound error = (Compound) getExceptionTerm();
		return error.arg(1);
	}
	
	@Override
	public String getMessage() {
		StringBuilder sb = new StringBuilder(super.getMessage());
		sb.append("Logtalk exception term: " + getLgtExceptionTerm());
		return sb.toString();
	}
	
}
