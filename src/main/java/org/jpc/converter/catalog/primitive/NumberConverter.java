package org.jpc.converter.catalog.primitive;

import java.lang.reflect.Type;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.text.NumberFormat;
import java.text.ParseException;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;

import org.jpc.Jpc;
import org.jpc.converter.JpcConversionException;
import org.jpc.converter.JpcConverter;
import org.jpc.term.Atom;
import org.jpc.term.FloatTerm;
import org.jpc.term.IntegerTerm;
import org.jpc.term.NumberTerm;
import org.jpc.term.Term;
import org.minitoolbox.reflection.ReflectionUtil;
import org.minitoolbox.reflection.typewrapper.TypeWrapper;

public class NumberConverter extends JpcConverter<Number, Term> {

	@Override
	public <T extends Term> T toTerm(Number number, Class<T> termClass, Jpc context) {
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
			throw new JpcConversionException();
		return (T) term;
	}

	@Override
	public Number fromTerm(Term term, Type type, Jpc context) {
		Number number = null;
		if(term instanceof IntegerTerm)
			number = ((IntegerTerm)term).longValue();
		else if(term instanceof FloatTerm)
			number = ((FloatTerm)term).doubleValue();
		else if(term instanceof Atom)
			try {
				number = NumberFormat.getInstance().parse(((Atom) term).getName());
			} catch (ParseException e) {
				throw new JpcConversionException();
			}
		else
			throw new JpcConversionException();
		
		if(TypeWrapper.wrap(type).isAssignableFrom(number.getClass()))
			return number;
		else {
			if(Long.class.equals(type))
				return new Long(number.longValue());
			else if(Integer.class.equals(type))
				return new Integer(number.intValue());
			else if(Short.class.equals(type))
				return new Short(number.shortValue());
			else if(Byte.class.equals(type))
				return new Byte(number.byteValue());
			else if(Double.class.equals(type))
				return new Double(number.doubleValue());
			else if(Float.class.equals(type))
				return new Float(number.floatValue());
			else if(BigInteger.class.equals(type))
				return new BigInteger(new Long(number.longValue()).toString());
			else if(BigDecimal.class.equals(type))
				return new BigDecimal(number.doubleValue());
			else if(AtomicInteger.class.equals(type))
				return new AtomicInteger(number.intValue());
			else if(AtomicLong.class.equals(type))
				return new AtomicLong(number.longValue());
			else 
				throw new JpcConversionException();
		}
	}
	
}
