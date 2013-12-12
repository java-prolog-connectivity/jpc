package org.jpc.error.handling;

import org.jconverter.converter.ConversionException;
import org.jpc.Jpc;
import org.jpc.term.Term;

/**
 * Handles an error term attempting to convert it, according to a given jpc context, to an instance of RuntimeException.
 * If it succeeds, throws the obtained exception. Otherwise returns false.
 * @author sergioc
 *
 */
public class JpcConverterErrorHandler implements ErrorHandler {
	
	@Override
	public boolean handle(Term errorTerm, Term goal, Jpc context) {
		RuntimeException ex;
		try {
			ex = context.fromTerm(errorTerm, RuntimeException.class);
		} catch(ConversionException ce) {
			return false;
		}
		throw ex;
	}

}
