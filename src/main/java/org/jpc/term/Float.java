package org.jpc.term;

import org.jpc.util.salt.TermContentHandler;
import org.jpc.term.visitor.TermVisitor;

import com.google.common.base.Function;


/**
 * A class reifying a logic float term
 * @author scastro
 *
 */
public final class Float extends Number {

	/**
	 * @param   value  This FloatTerm's (double) value
	 */
	public Float(double value) {
		super(value);
	}

	@Override
	public void accept(TermVisitor termVisitor) {
		termVisitor.visitFloat(this);
	}

	@Override
	protected void basicRead(TermContentHandler contentHandler, Function<Term, Term> termExpander) {
		contentHandler.startFloatTerm((Double)value);
	}


}
