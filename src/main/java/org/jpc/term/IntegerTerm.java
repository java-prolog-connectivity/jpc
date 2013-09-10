package org.jpc.term;

import java.util.Map;

import org.jpc.salt.TermContentHandler;
import org.jpc.term.visitor.TermVisitor;

/**
 * A class reifying a logic integer term
 * @author scastro
 *
 */
public final class IntegerTerm extends NumberTerm {

	/**
	 * @param   value  This Integer's (long) value
	 */
	public IntegerTerm(long value) {
		super(value);
	}
	
	/**
	 * Two IntegerTerms are equal if they are the same object, or their values are equal
	 * 
	 * @param   obj  The Object to compare
	 * @return  true if the Object satisfies the above condition
	 */
	@Override
	public boolean equals(Object obj) {
		return this == obj || (obj instanceof IntegerTerm && value == ((IntegerTerm) obj).value);
	}

	
	@Override
	public void accept(TermVisitor termVisitor) {
		termVisitor.visitInteger(this);
	}

	@Override
	public void basicRead(TermContentHandler contentHandler, Map<Term,Term> replacements) {
		contentHandler.startIntegerTerm((Long)value);
	}
}
