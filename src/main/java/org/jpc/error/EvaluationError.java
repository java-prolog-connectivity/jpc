package org.jpc.error;

import org.jpc.term.Compound;

public class EvaluationError extends IsoPrologError {
	
	public EvaluationError(Compound exceptionTerm) {
		super(exceptionTerm);
	}

}
