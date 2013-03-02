package org.jpc.exception;

import org.jpc.engine.prolog.PrologEngine;
import org.jpc.term.Term;

public interface ExceptionHandler {
	
	public abstract boolean handle(PrologEngine prologEngine, Term goal, Term exceptionTerm);

}
