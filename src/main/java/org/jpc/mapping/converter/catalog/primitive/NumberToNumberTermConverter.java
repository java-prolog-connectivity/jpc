package org.jpc.mapping.converter.catalog.primitive;

import static org.jconverter.converter.ConversionGoal.conversionGoal;

import org.jconverter.converter.DelegateConversionException;
import org.jconverter.converter.TypeDomain;
import org.jpc.Jpc;
import org.jpc.mapping.converter.FromTermConverter;
import org.jpc.mapping.converter.ToTermConverter;
import org.jpc.internal.reflection.ReflectionUtil;
import org.jpc.term.Float;
import org.jpc.term.Integer;
import org.jpc.term.Number;
import org.jpc.term.Term;


public class NumberToNumberTermConverter<T extends java.lang.Number, U extends Number> implements ToTermConverter<T, U>, FromTermConverter<U, T> {

	@Override
	public T fromTerm(U term, TypeDomain target, Jpc context) {
		java.lang.Number number = term.getValue();
		return context.convert(number, target);
	}

	@Override
	public U toTerm(T number, TypeDomain target, Jpc context) {
		Term term = null;
		if(target.getRawClass().equals(Number.class)) {
			if(ReflectionUtil.isFloatingPoint(number))
				term = new Float(number.doubleValue());
			else {
				term = new Integer(number.longValue());
			}
		} else if(target.getRawClass().equals(Integer.class)) {
			term = new Integer(number.longValue());
		} else if(target.getRawClass().equals(Float.class)) {
			term = new Float(number.doubleValue());
		} else {
			throw new DelegateConversionException(conversionGoal(number, target));
		}
		return (U) term;
	}

}
