package org.jpc.term;

import org.jpc.JpcException;
import org.jpc.engine.visitor.AbstractJpcVisitor;

/**
 * A class reifying a logic integer term
 * DISCLAIMER: In the current version many methods in this class have been copied or adapted from the class jpl.Integer in the JPL library.
 * @author scastro
 *
 */
public class IntegerTerm extends Term {

	/**
	 * the Integer's immutable long value
	 */
	protected final long value;
	
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
	public boolean hasFunctor(TermAdaptable nameTerm, int arity) {
		return termEquivalent(nameTerm) && arity == 0;
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
	
	/**
	 * Two IntegerTerms are equal if they are the same object, or their values are equal
	 * 
	 * @param   obj  The Object to compare
	 * @return  true if the Object satisfies the above condition
	 */
	public final boolean equals(Object obj) {
		return this == obj || (obj instanceof IntegerTerm && value == ((IntegerTerm) obj).value);
	}

	@Override
	public void accept(AbstractJpcVisitor termVisitor) {
		termVisitor.visitInteger(this);
	}
}
