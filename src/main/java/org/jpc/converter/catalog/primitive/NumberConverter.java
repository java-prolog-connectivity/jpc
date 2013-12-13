package org.jpc.converter.catalog.primitive;

import java.lang.reflect.Type;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.text.NumberFormat;
import java.text.ParseException;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;

import org.jconverter.converter.ConversionException;
import org.jpc.Jpc;
import org.jpc.converter.FromTermConverter;
import org.jpc.converter.ToTermConverter;
import org.jpc.term.Atom;
import org.jpc.term.FloatTerm;
import org.jpc.term.IntegerTerm;
import org.jpc.term.NumberTerm;
import org.jpc.term.Term;
import org.minitoolbox.reflection.ReflectionUtil;
import org.minitoolbox.reflection.typewrapper.TypeWrapper;

public class NumberConverter<T extends Number, U extends Term> implements ToTermConverter<T, U>, FromTermConverter<U, T> {

	@Override
	public U toTerm(T number, Class<U> termClass, Jpc context) {
		Term term = null;
		if(termClass.isAssignableFrom(NumberTerm.class)) {
			if(ReflectionUtil.isFloatingPoint(number))
				term = new FloatTerm(number.doubleValue());
			else
				term = new IntegerTerm(number.longValue());
		} else if(termClass.equals(IntegerTerm.class)) {
			term = new IntegerTerm(number.longValue());
		} else if(termClass.equals(FloatTerm.class)) {
			term = new FloatTerm(number.doubleValue());
		} else if(termClass.equals(Atom.class)) {
			term = new Atom(number.toString());
		} else
			throw new ConversionException();
		return (U) term;
	}

	@Override
	public T fromTerm(U term, Type type, Jpc context) {
		Number number = null;
		if(term instanceof NumberTerm)
			number = ((NumberTerm)term).getValue();
		else if(term instanceof Atom)
			try {
				number = NumberFormat.getInstance().parse(((Atom) term).getName());
			} catch (ParseException e) {
				throw new ConversionException();
			}
		else
			throw new ConversionException();
		
		if(!TypeWrapper.wrap(type).isAssignableFrom(number.getClass())) {
			if(Long.class.equals(type))
				number = new Long(number.longValue());
			else if(Integer.class.equals(type))
				number = new Integer(number.intValue());
			else if(Short.class.equals(type))
				number = new Short(number.shortValue());
			else if(Byte.class.equals(type))
				number = new Byte(number.byteValue());
			else if(Double.class.equals(type))
				number = new Double(number.doubleValue());
			else if(Float.class.equals(type))
				number = new Float(number.floatValue());
			else if(BigInteger.class.equals(type))
				number = new BigInteger(new Long(number.longValue()).toString());
			else if(BigDecimal.class.equals(type))
				number = new BigDecimal(number.doubleValue());
			else if(AtomicInteger.class.equals(type))
				number = new AtomicInteger(number.intValue());
			else if(AtomicLong.class.equals(type))
				number = new AtomicLong(number.longValue());
			else 
				throw new ConversionException();
		}
		return (T) number;
	}
	
}
