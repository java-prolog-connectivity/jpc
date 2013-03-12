package org.jpc.error;

import org.jpc.term.Term;

public class InstantiationError extends IsoPrologError {

	public static boolean isInstantiationError(Term term) {
		return true;
	}
	
	public InstantiationError(Term exceptionTerm) {
		super(exceptionTerm);
	}

}
