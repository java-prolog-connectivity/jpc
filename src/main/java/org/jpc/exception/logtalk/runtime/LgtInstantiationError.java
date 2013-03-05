package org.jpc.exception.logtalk.runtime;

import org.jpc.term.Term;

public class LgtInstantiationError extends LgtRunTimeException {

	public LgtInstantiationError(Term exceptionTerm) {
		super(exceptionTerm);
	}
	
	public LgtInstantiationError(Term exceptionTerm, Term goal) {
		super(exceptionTerm, goal);
	}
	
}
