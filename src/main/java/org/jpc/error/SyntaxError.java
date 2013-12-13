package org.jpc.error;

import org.jpc.term.Compound;

public class SyntaxError extends IsoPrologError {

	public SyntaxError(Compound exceptionTerm) {
		super(exceptionTerm);
	}

}
