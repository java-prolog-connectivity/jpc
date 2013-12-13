package org.jpc.error;

import org.jpc.term.Compound;

public class InstantiationError extends IsoPrologError {

	public InstantiationError(Compound exceptionTerm) {
		super(exceptionTerm);
	}

}
