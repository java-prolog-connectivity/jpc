package org.jpc.term;

import org.jpc.engine.visitor.AbstractTermVisitor;

/**
 * A class reifying a logic float term
 * Disclaimer: Some methods were inspired or taken from the JPL library
 * @author scastro
 *
 */
public class FloatTerm extends AbstractTerm {

	protected final double value;
	
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
	public boolean hasFunctor(TermAdaptable nameTerm, int arity) {
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
		return this == obj || (obj instanceof FloatTerm && ((FloatTerm)obj).canEquals(this) && value == ((FloatTerm) obj).value);
	}

	/**
	 * Any class overriding equals should override this method
	 * @param obj the object to compare
	 * @return whether this instance can equals the object sent as parameter
	 */
	public boolean canEquals(Object obj) {
		return obj instanceof FloatTerm;
	}

	@Override
	public void accept(AbstractTermVisitor termVisitor) {
		termVisitor.visitFloat(this);
	}

}
