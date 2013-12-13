package org.jpc.error;

import org.jpc.term.Compound;

public class ExistenceError extends IsoPrologError {
	
	public ExistenceError(Compound exceptionTerm) {
		super(exceptionTerm);
	}

}
