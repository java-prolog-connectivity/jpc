package org.jpc.converter.catalog.primitive;

import java.lang.reflect.Type;

import org.jconverter.converter.ConversionException;
import org.jpc.Jpc;
import org.jpc.converter.FromTermConverter;
import org.jpc.converter.ToTermConverter;
import org.jpc.term.FloatTerm;
import org.jpc.term.IntegerTerm;
import org.jpc.term.NumberTerm;

public class StringToNumberTermConverter<T extends NumberTerm> implements ToTermConverter<String, T>, FromTermConverter<T, String> {

	@Override
	public String fromTerm(T term, Type targetType, Jpc context) {
		return term.toString();
	}

	@Override
	public T toTerm(String source, Class<T> termClass, Jpc context) {
		T term;
		try {
			if(termClass.equals(IntegerTerm.class))
				term = (T) new IntegerTerm(Long.parseLong(source));
			else
				term = (T) new FloatTerm(Float.parseFloat(source));
		} catch(NumberFormatException e) {
			throw new ConversionException(source.getClass(), termClass, e);
		}
		
		return term;
	}

}
