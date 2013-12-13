package org.jpc.error;

import org.jpc.term.Compound;

public class ResourceError extends IsoPrologError {

	public ResourceError(Compound exceptionTerm) {
		super(exceptionTerm);
	}

}
