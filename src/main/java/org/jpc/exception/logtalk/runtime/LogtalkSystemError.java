package org.jpc.exception.logtalk.runtime;

import org.jpc.term.Term;

public class LogtalkSystemError extends LogtalkRunTimeException {

	public LogtalkSystemError(Term exceptionTerm) {
		super(exceptionTerm);
	}
	
	public LogtalkSystemError(Term exceptionTerm, Term goal) {
		super(exceptionTerm, goal);
	}
	
}
