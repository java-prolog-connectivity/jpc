package org.jpc.exception.logtalk.runtime;

import org.jpc.term.Term;

public class LgtEvaluationError extends LgtRunTimeException {

	public LgtEvaluationError(Term exceptionTerm) {
		super(exceptionTerm);
	}
	
	public LgtEvaluationError(Term exceptionTerm, Term goal) {
		super(exceptionTerm, goal);
	}
	
}
