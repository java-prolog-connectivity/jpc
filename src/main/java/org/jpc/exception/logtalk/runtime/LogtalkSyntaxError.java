package org.jpc.exception.logtalk.runtime;

import org.jpc.term.Term;

public class LogtalkSyntaxError extends LogtalkRunTimeException {

	public LogtalkSyntaxError(Term exceptionTerm) {
		super(exceptionTerm);
	}
	
	public LogtalkSyntaxError(Term exceptionTerm, Term goal) {
		super(exceptionTerm, goal);
	}
	
}
