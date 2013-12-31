package org.jpc.term;

import org.jpc.engine.prolog.OperatorsContext;
import org.jpc.term.compiled.CompilationContext;


public abstract class NumberTerm extends Term {

	/**
	 * The number term immutable value
	 */
	protected final Number value;
	
	public NumberTerm(Number number) {
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
	public String toEscapedString() {
		return value.toString();
	}
	
	@Override
	public boolean hasFunctor(Functor functor) {
		return functor.getArity() == 0 && termEquals(functor.getName());
	}

	@Override
	public boolean isGround() {
		return true;
	}
	
	@Override
	protected Term compile(int clauseId, CompilationContext context) {
		return this;
	}

	@Override
	protected Term compileForQuery(CompilationContext context) {
		return this;
	}

	@Override
	public Term forEnvironment(int environmentId) {
		return this;
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

}
