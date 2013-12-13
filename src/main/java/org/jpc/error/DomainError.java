package org.jpc.error;

import org.jpc.term.Compound;

public class DomainError extends IsoPrologError {
	
	public DomainError(Compound exceptionTerm) {
		super(exceptionTerm);
	}
	
}
