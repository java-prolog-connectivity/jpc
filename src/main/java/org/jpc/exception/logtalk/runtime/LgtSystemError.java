package org.jpc.exception.logtalk.runtime;

import org.jpc.term.Term;

public class LgtSystemError extends LgtRunTimeException {

	public LgtSystemError(Term exceptionTerm) {
		super(exceptionTerm);
	}
	
	public LgtSystemError(Term exceptionTerm, Term goal) {
		super(exceptionTerm, goal);
	}
	
}
