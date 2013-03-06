package org.jpc.exception.logtalk.runtime;

import org.jpc.term.Term;

public class LogtalkPermissionError extends LogtalkRunTimeException {

	public LogtalkPermissionError(Term exceptionTerm) {
		super(exceptionTerm);
	}
	
	public LogtalkPermissionError(Term exceptionTerm, Term goal) {
		super(exceptionTerm, goal);
	}
	
}
