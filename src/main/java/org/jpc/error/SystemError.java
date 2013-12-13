package org.jpc.error;

import org.jpc.term.Compound;

public class SystemError extends IsoPrologError {

	public SystemError(Compound exceptionTerm) {
		super(exceptionTerm);
	}

}
