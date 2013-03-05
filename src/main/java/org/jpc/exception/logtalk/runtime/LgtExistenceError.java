package org.jpc.exception.logtalk.runtime;

import org.jpc.term.Term;

public class LgtExistenceError extends LgtRunTimeException {

	public LgtExistenceError(Term exceptionTerm) {
		super(exceptionTerm);
	}
	
	public LgtExistenceError(Term exceptionTerm, Term goal) {
		super(exceptionTerm, goal);
	}
	
}
