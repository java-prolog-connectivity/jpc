package org.jpc.error;

import org.jpc.term.Compound;

public class TypeError extends IsoPrologError {

	public TypeError(Compound exceptionTerm) {
		super(exceptionTerm);
	}

}
