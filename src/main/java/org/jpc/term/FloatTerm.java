package org.jpc.term;

import org.jpc.engine.prolog.PrologEngine;
import org.jpc.salt.TermContentHandler;
import org.jpc.term.visitor.TermVisitor;


/**
 * A class reifying a logic float term
 * Disclaimer: Some methods were inspired or taken from the JPL library
 * @author scastro
 *
 */
public final class FloatTerm extends AbstractTerm {

	private final double value;
	
	/**
	 * This constructor creates a Float with the supplied 
	 * (double) value.
	 * 
	 * @param   value  this Float's value
	 */
	public FloatTerm(double value) {
		this.value = value;
	}
	

	/**
	 * returns the (double) value of this Float, converted to an int
	 * 
	 * @return the (double) value of this Float, converted to an int
	 */
	public final int intValue() {
		return (int) value;
	}

	/**
	 * returns the (double) value of this Float, converted to a long
	 * 
	 * @return the (double) value of this Float, converted to a long
	 */
	public final long longValue() {
		return (long) value;
	}

	/**
	 * returns the (double) value of this Float, converted to a float
	 * 
	 * @return the (double) value of this Float, converted to a float
	 */
	public final float floatValue() {
		return (float) value;
	}

	/**
	 * returns the (double) value of this Float
	 * 
	 * @return the (double) value of this Float
	 */
	public final double doubleValue() {
		return value;
	}
	
	/**
	 * Returns a Prolog source text representation of this FloatTerm
	 * 
	 * @return  a Prolog source text representation of this FloatTerm
	 */
	@Override
	public String toString() {
		return ""+value;
	}

	@Override
	public String toString(PrologEngine logicEngine) {
		return toString();
	}
	
	@Override
	public boolean hasFunctor(TermConvertable nameTerm, int arity) {
		return termEquals(nameTerm) && arity == 0;
	}

	@Override
	public int hashCode() {
		return Double.valueOf(value).hashCode();
	}
	
	/**
	 * Two FloatTerms are equal if they are the same object, or their values are equal
	 * 
	 * @param   obj  The Object to compare
	 * @return  true if the Object satisfies the above condition
	 */
	@Override
	public boolean equals(Object obj) {
		return this == obj || (obj instanceof FloatTerm && value == ((FloatTerm) obj).value);
	}

	@Override
	public void accept(TermVisitor termVisitor) {
		termVisitor.visitFloat(this);
	}

	@Override
	public void read(TermContentHandler contentHandler) {
		contentHandler.startFloatTerm(value);
	}


}
