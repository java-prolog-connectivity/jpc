package org.jpc.exception;

import org.jpc.term.Term;

public class LogtalkException extends PrologException {

	public LogtalkException(Term goal, Term exceptionTerm) {
		super(goal, exceptionTerm);
	}
	
	public LogtalkException(Term goal, Term exceptionTerm, String message) {
		super(goal, exceptionTerm, message);
	}

}
