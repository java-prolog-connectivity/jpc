package org.jpc.exception;

import org.jpc.engine.prolog.PrologEngine;
import org.jpc.term.Term;

public class RootExceptionHandlerManager extends ExceptionHandlerManager {
	
	@Override
	public boolean handle(PrologEngine prologEngine, Term exceptionTerm, Term goal) {
		if(!super.handle(prologEngine, exceptionTerm, goal))
			defaultHandling(prologEngine, goal, exceptionTerm);
		return true;
	}
	
	public void defaultHandling(PrologEngine prologEngine, Term goal, Term exceptionTerm) {
		throw new PrologException(exceptionTerm, goal);
	}
	
}
