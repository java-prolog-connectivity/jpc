package org.jpc.exception.logtalk.runtime;

import org.jpc.term.Term;

public class LgtSyntaxError extends LgtRunTimeException {

	public LgtSyntaxError(Term exceptionTerm) {
		super(exceptionTerm);
	}
	
	public LgtSyntaxError(Term exceptionTerm, Term goal) {
		super(exceptionTerm, goal);
	}
	
}
