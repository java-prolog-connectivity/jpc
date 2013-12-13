package org.jpc.error;

import org.jpc.term.Compound;

public class PermissionError extends IsoPrologError {

	public PermissionError(Compound exceptionTerm) {
		super(exceptionTerm);
	}

}
