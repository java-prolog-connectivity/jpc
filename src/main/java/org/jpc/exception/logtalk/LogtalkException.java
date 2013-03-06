package org.jpc.exception.logtalk;

import org.jpc.exception.IsoPrologException;
import org.jpc.term.Term;

/**
 * @author sergioc
 *
 */
public class LogtalkException extends IsoPrologException {

	public LogtalkException(Term exceptionTerm) {
		super(exceptionTerm);
	}
	
	public LogtalkException(Term exceptionTerm, Term goal) {
		super(exceptionTerm, goal);
	}
	
}
