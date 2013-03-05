package org.jpc.exception.logtalk.compiletime;

import org.jpc.exception.logtalk.LogtalkException;
import org.jpc.term.Compound;
import org.jpc.term.Term;
import org.jpc.term.Variable;

public class LgtCompileTimeException extends LogtalkException {

	public LgtCompileTimeException(Term exceptionTerm) {
		super(exceptionTerm);
	}
	
	public LgtCompileTimeException(Term exceptionTerm, Term goal) {
		super(exceptionTerm, goal);
	}
	
	public Term getCulprit() {
		Compound error = (Compound) getExceptionTerm();
		return error.arg(2);
	}
	
	@Override
	public String getMessage() {
		StringBuilder sb = new StringBuilder(super.getMessage());
		Term culprit = getCulprit();
		if(!(culprit instanceof Variable))
			sb.append("Culprit: " + culprit);
		return sb.toString();
	}

}
