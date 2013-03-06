package org.jpc.exception.logtalk.runtime;

import org.jpc.term.Term;

public class LogtalkEvaluationError extends LogtalkRunTimeException {

	public LogtalkEvaluationError(Term exceptionTerm) {
		super(exceptionTerm);
	}
	
	public LogtalkEvaluationError(Term exceptionTerm, Term goal) {
		super(exceptionTerm, goal);
	}
	
}
