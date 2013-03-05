package org.jpc.exception.logtalk.runtime;

import org.jpc.term.Term;

public class LgtResourceError extends LgtRunTimeException {

	public LgtResourceError(Term exceptionTerm) {
		super(exceptionTerm);
	}
	
	public LgtResourceError(Term exceptionTerm, Term goal) {
		super(exceptionTerm, goal);
	}
	
}
