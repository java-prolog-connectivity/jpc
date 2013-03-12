package org.jpc.error;

import org.jpc.term.Term;

public class ResourceError extends IsoPrologError {

	public static boolean isResourceError(Term term) {
		return true;
	}
	
	public ResourceError(Term exceptionTerm) {
		super(exceptionTerm);
	}

}
