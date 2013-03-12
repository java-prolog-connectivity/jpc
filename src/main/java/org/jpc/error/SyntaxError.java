package org.jpc.error;

import org.jpc.term.Term;

public class SyntaxError extends IsoPrologError {

	public static boolean isSyntaxError(Term term) {
		return true;
	}
	
	public SyntaxError(Term exceptionTerm) {
		super(exceptionTerm);
	}

}
