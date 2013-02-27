package org.jpc.converter.fromterm;

import java.lang.reflect.Type;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;

import org.jpc.Jpc;
import org.jpc.converter.JpcConversionException;
import org.jpc.term.Atom;
import org.jpc.term.IntegerTerm;
import org.jpc.term.NumberTerm;
import org.jpc.term.Term;

public class TermToNumberConverter extends FromTermConverter<Number> {

	@Override
	public Number convert(Term term, Type type, Jpc context) {
		Double number = null;
		if(term instanceof NumberTerm)
			number = ((NumberTerm)term).doubleValue();
		else if(term instanceof Atom)
			number = Double.parseDouble(term.toString());
		else
			throw new JpcConversionException();
		
		if(Number.class.equals(type)) { //base the return value on the term type
			if(term instanceof IntegerTerm)
				return number.longValue();
			else
				return number;
		} else {
			if(AtomicInteger.class.equals(type))
				return new AtomicInteger(number.intValue());
			else if(AtomicLong.class.equals(type))
				return new AtomicLong(number.longValue());
			else if(BigDecimal.class.equals(type))
				return new BigDecimal(number);
			else if(BigInteger.class.equals(type))
				return new BigInteger(new Long(number.longValue()).toString());
			else if(Byte.class.equals(type))
				return new Byte(number.byteValue());
			else if(Double.class.equals(type))
				return new Double(number);
			else if(Float.class.equals(type))
				return new Float(number);
			else if(Integer.class.equals(type))
				return new Integer(number.intValue());
			else if(Long.class.equals(type))
				return new Long(number.longValue());
			else if(Short.class.equals(type))
				return new Short(number.shortValue());
			else
				throw new JpcConversionException();
		}
		
	}

	
	
}
