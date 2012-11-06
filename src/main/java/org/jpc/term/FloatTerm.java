package org.jpc.term;

import java.util.Collections;
import java.util.List;

import org.jpc.engine.visitor.AbstractJpcVisitor;

/**
 * A class reifying a logic float term
 * DISCLAIMER: In the current version many methods in this class have been copied or adapted from the class jpl.Float in the JPL library.
 * @author scastro
 *
 */
public class FloatTerm extends Term {

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
		return termEquivalent(nameTerm) && arity == 0;
	}

	/**
	 * Two FloatTerms are equal if they are the same object, or their values are equal
	 * 
	 * @param   obj  The Object to compare
	 * @return  true if the Object satisfies the above condition
	 */
	public final boolean equals(Object obj) {
		return this == obj || (obj instanceof FloatTerm && value == ((FloatTerm) obj).value);
	}


	@Override
	public void accept(AbstractJpcVisitor termVisitor) {
		termVisitor.visitFloat(this);
	}
}
