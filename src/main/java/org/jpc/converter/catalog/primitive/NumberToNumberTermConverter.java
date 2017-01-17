package org.jpc.converter.catalog.primitive;

import java.lang.reflect.Type;

import org.jconverter.converter.ConversionException;
import org.jpc.Jpc;
import org.jpc.converter.FromTermConverter;
import org.jpc.converter.ToTermConverter;
import org.jpc.internal.reflection.ReflectionUtil;
import org.jpc.term.Float;
import org.jpc.term.Integer;
import org.jpc.term.Number;
import org.jpc.term.Term;


public class NumberToNumberTermConverter<T extends java.lang.Number, U extends Number> implements ToTermConverter<T, U>, FromTermConverter<U, T> {

	@Override
	public T fromTerm(U term, Type targetType, Jpc context) {
		java.lang.Number number = term.getValue();
		return context.convert(number, targetType);
	}

	@Override
	public U toTerm(T number, Class<U> termClass, Jpc context) {
		Term term = null;
		if(termClass.equals(Number.class)) {
			if(ReflectionUtil.isFloatingPoint(number))
				term = new Float(number.doubleValue());
			else
				term = new Integer(number.longValue());
		} else if(termClass.equals(Integer.class)) {
			term = new Integer(number.longValue());
		} else if(termClass.equals(Float.class)) {
			term = new Float(number.doubleValue());
		} else
			throw new ConversionException();
		return (U) term;
	}

}
