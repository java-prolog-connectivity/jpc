package org.jpc.converter.catalog.primitive;

import java.lang.reflect.Type;

import org.jconverter.converter.ConversionException;
import org.jpc.Jpc;
import org.jpc.converter.FromTermConverter;
import org.jpc.converter.ToTermConverter;
import org.jpc.term.FloatTerm;
import org.jpc.term.IntegerTerm;
import org.jpc.term.NumberTerm;
import org.jpc.term.Term;
import org.minitoolbox.reflection.ReflectionUtil;

public class NumberToNumberTermConverter<T extends Number, U extends NumberTerm> implements ToTermConverter<T, U>, FromTermConverter<U, T> {

	@Override
	public T fromTerm(U term, Type targetType, Jpc context) {
		Number number = term.getValue();
		return context.convert(number, targetType);
	}

	@Override
	public U toTerm(T number, Class<U> termClass, Jpc context) {
		Term term = null;
		if(termClass.equals(NumberTerm.class)) {
			if(ReflectionUtil.isFloatingPoint(number))
				term = new FloatTerm(number.doubleValue());
			else
				term = new IntegerTerm(number.longValue());
		} else if(termClass.equals(IntegerTerm.class)) {
			term = new IntegerTerm(number.longValue());
		} else if(termClass.equals(FloatTerm.class)) {
			term = new FloatTerm(number.doubleValue());
		} else
			throw new ConversionException();
		return (U) term;
	}

}
