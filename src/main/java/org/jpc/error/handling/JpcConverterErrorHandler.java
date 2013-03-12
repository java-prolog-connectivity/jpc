package org.jpc.error.handling;

import org.jpc.Jpc;
import org.jpc.converter.JpcConversionException;
import org.jpc.engine.prolog.PrologEngine;
import org.jpc.term.Term;

/**
 * Handles an error term attempting to convert it, according to the default jpc context of the engine, to an instance of RuntimeException.
 * If it succeeds it throws the exception, otherwise returns false
 * @author sergioc
 *
 */
public class JpcConverterErrorHandler implements ErrorHandler {
	
	@Override
	public boolean handle(PrologEngine prologEngine, Term errorTerm, Term goal, Jpc context) {
		RuntimeException ex;
		try {
			ex = context.fromTerm(errorTerm, RuntimeException.class);
		} catch(JpcConversionException ce) {
			return false;
		}
		throw ex;
	}

}
