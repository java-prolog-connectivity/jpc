package org.jpc.error;

import org.jpc.term.Term;

public class EvaluationError extends IsoPrologError {

	public static boolean isEvaluationError(Term term) {
		return true;
	}
	
	public EvaluationError(Term exceptionTerm) {
		super(exceptionTerm);
	}

}
