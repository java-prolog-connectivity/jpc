package org.jpc.exception.logtalk.runtime;

import org.jpc.term.Term;

public class LgtRepresentationError extends LgtRunTimeException {

	public LgtRepresentationError(Term exceptionTerm) {
		super(exceptionTerm);
	}
	
	public LgtRepresentationError(Term exceptionTerm, Term goal) {
		super(exceptionTerm, goal);
	}
	
}
