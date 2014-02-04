package org.jpc.term;

import org.jpc.salt.TermContentHandler;
import org.jpc.term.visitor.TermVisitor;

import com.google.common.base.Function;

/**
 * A class reifying a logic integer term
 * @author scastro
 *
 */
public class IntegerTerm extends NumberTerm {

	/**
	 * @param   value  This IntegerTerm's (long) value
	 */
	public IntegerTerm(long value) {
		super(value);
	}
	
	@Override
	public void accept(TermVisitor termVisitor) {
		termVisitor.visitInteger(this);
	}

	@Override
	protected void basicRead(TermContentHandler contentHandler, Function<Term, Term> termExpander) {
		contentHandler.startIntegerTerm((Long)value);
	}

}
