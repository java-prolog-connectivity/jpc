package org.jpc.exception.logtalk;

import org.jpc.exception.PrologException;
import org.jpc.term.Term;

public class LogtalkException extends PrologException {

	public LogtalkException(Term goal, Term exceptionTerm) {
		super(goal, exceptionTerm);
	}
	
	public LogtalkException(Term goal, Term exceptionTerm, String message) {
		super(goal, exceptionTerm, message);
	}

}
