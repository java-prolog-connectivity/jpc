package org.jpc.term;

import org.jpc.engine.prolog.OperatorsContext;
import org.jpc.term.compiler.Environment;


public abstract class NumberTerm extends Term {

	/**
	 * The number term immutable value
	 */
	protected final Number value;
	
	NumberTerm(Number number) {
		this.value = number;
	}
	
	public Number getValue() {
		return value;
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
	public boolean isGround() {
		return true;
	}
	
	@Override
	public String toEscapedString() {
		return value.toString();
	}
	
	@Override
	public boolean hasFunctor(Functor functor) {
		return functor.getArity() == 0 && termEquals(functor.getName());
	}
	
	@Override
	public int hashCode() {
		return value.hashCode();
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
		return value.equals(other.value);
	}

	@Override
	public String toString(OperatorsContext operatorsContext) {
		return toString();
	}

	
	@Override
	public Term preCompile(Environment env) {
		return this;
	}

	@Override
	public Term prepareForQuery(Environment env) {
		return this;
	}

	@Override
	public Term prepareForFrame(Environment env) {
		return this;
	}
	
}
