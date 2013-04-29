package org.jpc.error.handling;

import org.jpc.Jpc;
import org.jpc.converter.catalog.error.IsoPrologErrorConverter;
import org.jpc.error.IsoPrologError;
import org.jpc.error.PrologError;
import org.jpc.term.Term;

public class RootErrorHandlerManager extends ErrorHandlerManager {
	
	@Override
	public boolean handle(Term errorTerm, Term goal, Jpc context) {
		if(!super.handle(errorTerm, goal, context))
			defaultHandling(errorTerm, goal, context);
		return true;
	}
	
	public void defaultHandling(Term errorTerm, Term goal, Jpc context) {
		if(IsoPrologErrorConverter.isIsoPrologError(errorTerm))
			throw new IsoPrologError(errorTerm);
		else
			throw new PrologError(errorTerm);
	}
	
}
