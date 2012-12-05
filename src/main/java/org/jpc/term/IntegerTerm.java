package org.jpc.term;

import org.jpc.JpcException;
import org.jpc.salt.ContentHandler;
import org.jpc.term.visitor.TermVisitor;

/**
 * A class reifying a logic integer term
 * Disclaimer: Some methods were inspired or taken from the JPL library
 * @author scastro
 *
 */
public final class IntegerTerm extends AbstractTerm {

	/**
	 * the Integer's immutable long value
	 */
	private final long value;
	
	/**
	 * @param   value  This Integer's (long) value
	 */
	public IntegerTerm(long value) {
		this.value = value;
	}
	
	/**
	 * Returns the value of this Integer as an int if possible, else throws a JPLException
	 * 
	 * @throws JPLException if the value of this Integer is too great to be represented as a Java int
	 * @return the int value of this Integer
	 */
	public final int intValue() {
		if (value < java.lang.Integer.MIN_VALUE || value > java.lang.Integer.MAX_VALUE) {
			throw new JpcException("cannot represent Integer value as an int");
		} else {
			return (int)value;
		}
	}

	/**
	 * Returns the value of this Integer as a long
	 * 
	 * @return the value of this Integer as a long
	 */
	public final long longValue() {
		return value;
	}
	
	/**
	 * Returns the value of this Integer converted to a float
	 * 
	 * @return the value of this Integer converted to a float
	 */
	public final float floatValue() {
		return value;
	}

	/**
	 * Returns the value of this Integer converted to a double
	 * 
	 * @return the value of this Integer converted to a double
	 */
	public final double doubleValue() {
		return value;
	}

	@Override
	public boolean hasFunctor(TermConvertable nameTerm, int arity) {
		return termEquals(nameTerm) && arity == 0;
	}
	
	/**
	 * Returns a Prolog source text representation of this IntegerTerm
	 * 
	 * @return  a Prolog source text representation of this IntegerTerm
	 */
	@Override
	public String toString() {
		return ""+value;
	}
	
	@Override
	public int hashCode() {
		return Long.valueOf(value).hashCode();
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
	public void read(ContentHandler contentHandler) {
		contentHandler.startIntegerTerm(value);
	}
}
