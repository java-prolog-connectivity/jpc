package org.jpc.error;

import org.jpc.converter.TermConvertable;
import org.jpc.term.Term;

/**
 * An exception in the Prolog side
 * @author sergioc
 *
 */
public class PrologError extends RuntimeException implements TermConvertable {

	protected Term errorTerm; //the error term thrown from the Prolog side

	public PrologError(Term errorTerm) {
		this.errorTerm = errorTerm;
	}

	@Override
	public Term asTerm() {
		return errorTerm;
	}

	@Override
	public String getMessage() {
		StringBuilder sb = new StringBuilder("Error Term: " + errorTerm);
		sb.append(". ");
		return sb.toString();
	}
	
}
