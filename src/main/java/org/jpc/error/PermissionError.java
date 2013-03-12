package org.jpc.error;

import org.jpc.term.Term;

public class PermissionError extends IsoPrologError {

	public static boolean isPermissionError(Term term) {
		return true;
	}
	
	public PermissionError(Term exceptionTerm) {
		super(exceptionTerm);
	}

}
