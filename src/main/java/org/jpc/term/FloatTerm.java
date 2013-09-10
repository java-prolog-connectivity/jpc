package org.jpc.term;

import java.util.Map;

import org.jpc.salt.TermContentHandler;
import org.jpc.term.visitor.TermVisitor;


/**
 * A class reifying a logic float term
 * @author scastro
 *
 */
public final class FloatTerm extends NumberTerm {

	public FloatTerm(double value) {
		super(value);
	}

	@Override
	public void accept(TermVisitor termVisitor) {
		termVisitor.visitFloat(this);
	}

	@Override
	public void basicRead(TermContentHandler contentHandler, Map<Term,Term> replacements) {
		contentHandler.startFloatTerm((Double)value);
	}


}
