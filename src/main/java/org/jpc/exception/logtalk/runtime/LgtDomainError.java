package org.jpc.exception.logtalk.runtime;

import org.jpc.term.Term;

public class LgtDomainError extends LgtRunTimeException {

	public LgtDomainError(Term exceptionTerm) {
		super(exceptionTerm);
	}
	
	public LgtDomainError(Term exceptionTerm, Term goal) {
		super(exceptionTerm, goal);
	}
	
}
