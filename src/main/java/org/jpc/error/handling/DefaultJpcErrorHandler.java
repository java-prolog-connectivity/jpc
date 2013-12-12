package org.jpc.error.handling;

import org.jpc.Jpc;
import org.jpc.converter.catalog.error.PrologErrorConverter;
import org.jpc.error.PrologError;
import org.jpc.term.Term;

public class DefaultJpcErrorHandler extends ErrorHandlerManager {

	public DefaultJpcErrorHandler() {
		register(new JpcConverterErrorHandler());
	}
	
	@Override
	public boolean handle(Term errorTerm, Term goal, Jpc context) {
		if(!super.handle(errorTerm, goal, context)) //attempts to use registered error handlers
			defaultHandling(errorTerm, goal, context); //invoking the default handling if no registered error handler could handle the error.
		return true;
	}
	
	private void defaultHandling(Term errorTerm, Term goal, Jpc context) {
		throw new PrologErrorConverter().fromTerm(errorTerm, PrologError.class, context); //Generic (non ISO) Prolog error.
	}

}
