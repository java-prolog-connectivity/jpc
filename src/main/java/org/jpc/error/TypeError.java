package org.jpc.error;

import org.jpc.term.Term;

public class TypeError extends IsoPrologError {

	public static boolean isTypeError(Term term) {
		return true;
	}
	
	public TypeError(Term exceptionTerm) {
		super(exceptionTerm);
	}

}
