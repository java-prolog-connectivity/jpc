package org.jpc.exception.logtalk.runtime;

import org.jpc.term.Term;

public class LogtalkExistenceError extends LogtalkRunTimeException {

	public LogtalkExistenceError(Term exceptionTerm) {
		super(exceptionTerm);
	}
	
	public LogtalkExistenceError(Term exceptionTerm, Term goal) {
		super(exceptionTerm, goal);
	}
	
}
