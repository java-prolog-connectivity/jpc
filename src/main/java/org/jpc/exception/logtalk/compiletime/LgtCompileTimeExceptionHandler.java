package org.jpc.exception.logtalk.compiletime;

import org.jpc.engine.prolog.PrologEngine;
import org.jpc.exception.MatchExceptionHandler;
import org.jpc.exception.PrologException;
import org.jpc.term.Term;

public class LgtCompileTimeExceptionHandler extends MatchExceptionHandler {

	public LgtCompileTimeExceptionHandler(Term exceptionTerm) {
		super(exceptionTerm);
	}

	public void onMatch(PrologEngine prologEngine, Term unifiedExceptionTerm, Term goal) {
		throw new LgtCompileTimeException(unifiedExceptionTerm, goal);
	}
	
}
