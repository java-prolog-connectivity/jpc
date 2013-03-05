package org.jpc.exception.logtalk.runtime;

import org.jpc.term.Term;

public class LgtPermissionError extends LgtRunTimeException {

	public LgtPermissionError(Term exceptionTerm) {
		super(exceptionTerm);
	}
	
	public LgtPermissionError(Term exceptionTerm, Term goal) {
		super(exceptionTerm, goal);
	}
	
}
