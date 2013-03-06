package org.jpc.exception.logtalk.runtime;

import org.jpc.term.Term;

public class LogtalkDomainError extends LogtalkRunTimeException {

	public LogtalkDomainError(Term exceptionTerm) {
		super(exceptionTerm);
	}
	
	public LogtalkDomainError(Term exceptionTerm, Term goal) {
		super(exceptionTerm, goal);
	}
	
}
