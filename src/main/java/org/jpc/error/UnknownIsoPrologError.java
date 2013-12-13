package org.jpc.error;

import org.jpc.term.Compound;

/**
 * A class representing a Prolog error that does not have a known representation in the Java side.
 * @author sergioc
 *
 */
public class UnknownIsoPrologError extends IsoPrologError {

	public UnknownIsoPrologError(Compound exceptionTerm) {
		super(exceptionTerm);
	}

}
