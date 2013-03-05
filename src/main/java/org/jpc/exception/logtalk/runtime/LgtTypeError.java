package org.jpc.exception.logtalk.runtime;

import org.jpc.term.Term;

public class LgtTypeError extends LgtRunTimeException {

	public LgtTypeError(Term exceptionTerm) {
		super(exceptionTerm);
	}
	
	public LgtTypeError(Term exceptionTerm, Term goal) {
		super(exceptionTerm, goal);
	}
	
}
