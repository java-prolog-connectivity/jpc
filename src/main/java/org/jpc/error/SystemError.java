package org.jpc.error;

import org.jpc.term.Term;

public class SystemError extends IsoPrologError {

	public static boolean isSystemError(Term term) {
		return true;
	}
	
	public SystemError(Term exceptionTerm) {
		super(exceptionTerm);
	}

}
