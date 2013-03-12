package org.jpc.error;

import org.jpc.term.Term;

public class RepresentationError extends IsoPrologError {

	public static boolean isRepresentationError(Term term) {
		return true;
	}
	
	public RepresentationError(Term exceptionTerm) {
		super(exceptionTerm);
	}

}
