package org.jpc.exception.logtalk.runtime;

import org.jpc.term.Term;

public class LogtalkResourceError extends LogtalkRunTimeException {

	public LogtalkResourceError(Term exceptionTerm) {
		super(exceptionTerm);
	}
	
	public LogtalkResourceError(Term exceptionTerm, Term goal) {
		super(exceptionTerm, goal);
	}
	
}
