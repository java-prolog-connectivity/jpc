package org.jpc.term;

import org.jpc.engine.prolog.PrologEngine;

public abstract class NumberTerm extends AbstractTerm {

	/**
	 * The number term immutable value
	 */
	protected final Number value;
	
	public NumberTerm(Number number) {
		this.value = number;
	}
	
	public final int intValue() {
		return value.intValue();
	}
	
	public final long longValue() {
		return value.longValue();
	}
	
	public final float floatValue() {
		return value.floatValue();
	}
	
	public final double doubleValue() {
		return value.doubleValue();
	}
	
	@Override
	public String toEscapedString() {
		return value.toString();
	}
	
	@Override
	public boolean hasFunctor(Term nameTerm, int arity) {
		return equals(nameTerm) && arity == 0;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((value == null) ? 0 : value.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		NumberTerm other = (NumberTerm) obj;
		if (value == null) {
			if (other.value != null)
				return false;
		} else if (!value.equals(other.value))
			return false;
		return true;
	}


}
