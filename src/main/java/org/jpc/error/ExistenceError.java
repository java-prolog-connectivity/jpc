package org.jpc.error;

import org.jpc.term.Term;

public class ExistenceError extends IsoPrologError {

	public static boolean isExistenceError(Term term) {
		return true;
	}
	
	public ExistenceError(Term exceptionTerm) {
		super(exceptionTerm);
	}

}
