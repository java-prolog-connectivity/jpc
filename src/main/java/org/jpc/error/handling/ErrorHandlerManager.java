package org.jpc.error.handling;

import java.util.ArrayList;
import java.util.List;

import org.jpc.Jpc;
import org.jpc.term.Term;

public class ErrorHandlerManager implements ErrorHandler {

	private List<ErrorHandler> errorHandlers;
	
	public ErrorHandlerManager() {
		errorHandlers = new ArrayList<>();
	}
	
	public void register(ErrorHandler errorHandler ) {
		errorHandlers.add(0, errorHandler);
	}

	@Override
	public boolean handle(Term errorTerm, Term goal, Jpc context) {
		for(ErrorHandler errorHandler : errorHandlers) {
			if(errorHandler.handle(errorTerm, goal, context))
				return true;
		}
		return false;
	}

}

