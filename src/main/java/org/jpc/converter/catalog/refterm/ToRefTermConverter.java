package org.jpc.converter.catalog.refterm;

import static org.jconverter.converter.ConversionGoal.conversionGoal;

import org.jconverter.converter.DelegateConversionException;
import org.jconverter.converter.TypeDomain;
import org.jpc.Jpc;
import org.jpc.converter.ToTermConverter;
import org.jpc.term.Compound;

public class ToRefTermConverter<T> implements ToTermConverter<T, Compound> {

	@Override
	public Compound toTerm(T object, TypeDomain target, Jpc context) {
		Compound compound = context.refTerm(object);
		if(compound == null) {
			throw new DelegateConversionException(conversionGoal(object, target));
		}
		return compound;
	}

}
