package org.jpc.exception;

import org.jpc.engine.prolog.PrologEngine;
import org.jpc.term.Term;

public class RootExceptionHandlerManager extends ExceptionHandlerManager {
	
	@Override
	public boolean handle(PrologEngine prologEngine, Term goal, Term exceptionTerm) {
		if(!super.handle(prologEngine, goal, exceptionTerm))
			defaultHandling(prologEngine, goal, exceptionTerm);
		return true;
	}
	
	public void defaultHandling(PrologEngine prologEngine, Term goal, Term exceptionTerm) {
		throw new PrologException(goal, exceptionTerm);
	}
	
}
