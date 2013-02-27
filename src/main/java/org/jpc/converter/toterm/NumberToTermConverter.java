package org.jpc.converter.toterm;

import org.jpc.Jpc;
import org.jpc.term.FloatTerm;
import org.jpc.term.IntegerTerm;
import org.jpc.term.Term;
import org.minitoolbox.reflection.ReflectionUtil;

public class NumberToTermConverter extends ToTermConverter<Number> {

	@Override
	public Term convert(Number number, Jpc context) {
		Term term = null;
		if(ReflectionUtil.isFloatingPoint(number))
			term = new FloatTerm(((Number)number).doubleValue());
		else
			term = new IntegerTerm(((Number)number).longValue());
		return term;
	}

}
