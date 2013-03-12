package org.jpc.error;

import org.jpc.term.Term;

public class DomainError extends IsoPrologError {
	
	public DomainError(Term exceptionTerm) {
		super(exceptionTerm);
	}
	
}
