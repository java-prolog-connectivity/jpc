package org.jpc.exception.logtalk.runtime;

import org.jpc.term.Term;

public class LogtalkTypeError extends LogtalkRunTimeException {

	public LogtalkTypeError(Term exceptionTerm) {
		super(exceptionTerm);
	}
	
	public LogtalkTypeError(Term exceptionTerm, Term goal) {
		super(exceptionTerm, goal);
	}
	
}
