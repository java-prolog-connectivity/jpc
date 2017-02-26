package org.jpc.mapping.converter.catalog.primitive;

import static org.jconverter.converter.ConversionGoal.conversionGoal;

import org.jconverter.converter.DelegateConversionException;
import org.jconverter.converter.TypeDomain;
import org.jpc.Jpc;
import org.jpc.mapping.converter.FromTermConverter;
import org.jpc.mapping.converter.ToTermConverter;
import org.jpc.term.Float;
import org.jpc.term.Integer;
import org.jpc.term.Number;

public class StringToNumberTermConverter<T extends Number> implements ToTermConverter<String, T>, FromTermConverter<T, String> {

	@Override
	public String fromTerm(T term, TypeDomain target, Jpc context) {
		return term.toString();
	}

	@Override
	public T toTerm(String source, TypeDomain target, Jpc context) {
		T term;
		try {
			if(target.getRawClass().equals(Integer.class))
				term = (T) new Integer(Long.parseLong(source));
			else
				term = (T) new Float(java.lang.Float.parseFloat(source));
		} catch(NumberFormatException e) {
			throw new DelegateConversionException(conversionGoal(source, target), e);
		}
		
		return term;
	}

}
