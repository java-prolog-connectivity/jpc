package org.jpc.exception;

import java.util.ArrayList;
import java.util.List;

import org.jpc.engine.prolog.PrologEngine;
import org.jpc.term.Term;

public class ExceptionHandlerManager implements ExceptionHandler {

	private List<ExceptionHandler> exceptionHandlers;
	
	public ExceptionHandlerManager() {
		exceptionHandlers = new ArrayList<>();
	}
	
	public void register(ExceptionHandler exceptionHandler ) {
		exceptionHandlers.add(0, exceptionHandler);
	}

	@Override
	public boolean handle(PrologEngine prologEngine, Term exceptionTerm, Term goal) {
		for(ExceptionHandler exceptionHandler : exceptionHandlers) {
			if(exceptionHandler.handle(prologEngine, exceptionTerm, goal))
				return true;
		}
		return false;
	}

}

