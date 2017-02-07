package org.jpc.term;

import org.jpc.util.salt.TermContentHandler;
import org.jpc.term.visitor.TermVisitor;

import com.google.common.base.Function;

/**
 * A class reifying a logic integer term
 * @author scastro
 *
 */
public final class Integer extends Number {

	/**
	 * @param   value  This IntegerTerm's (long) value
	 */
	public Integer(long value) {
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
