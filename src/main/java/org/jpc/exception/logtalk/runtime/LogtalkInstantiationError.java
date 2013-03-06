package org.jpc.exception.logtalk.runtime;

import org.jpc.term.Term;

public class LogtalkInstantiationError extends LogtalkRunTimeException {

	public LogtalkInstantiationError(Term exceptionTerm) {
		super(exceptionTerm);
	}
	
	public LogtalkInstantiationError(Term exceptionTerm, Term goal) {
		super(exceptionTerm, goal);
	}
	
}
