package org.jpc.converter.catalog.jrefterm;

import org.jconverter.converter.ConversionException;
import org.jpc.Jpc;
import org.jpc.converter.ToTermConverter;
import org.jpc.term.Compound;

public class ToJRefTermConverter<T> implements ToTermConverter<T, Compound> {

	@Override
	public Compound toTerm(T object, Class<Compound> termClass, Jpc context) {
		Compound compound = context.jRefTerm(object);
		if(compound == null)
			throw new ConversionException();
		return compound;
	}

}
