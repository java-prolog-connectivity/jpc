package org.jpc.exception.logtalk.compiletime;

import org.jpc.exception.logtalk.LogtalkException;
import org.jpc.term.Compound;
import org.jpc.term.Term;
import org.jpc.term.Variable;

public class LogtalkCompileTimeException extends LogtalkException {

	public LogtalkCompileTimeException(Term exceptionTerm) {
		super(exceptionTerm);
	}
	
	public LogtalkCompileTimeException(Term exceptionTerm, Term goal) {
		super(exceptionTerm, goal);
	}

}
