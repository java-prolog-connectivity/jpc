package org.jpc.exception.logtalk.runtime;

import org.jpc.term.Term;

public class LogtalkRepresentationError extends LogtalkRunTimeException {

	public LogtalkRepresentationError(Term exceptionTerm) {
		super(exceptionTerm);
	}
	
	public LogtalkRepresentationError(Term exceptionTerm, Term goal) {
		super(exceptionTerm, goal);
	}
	
}