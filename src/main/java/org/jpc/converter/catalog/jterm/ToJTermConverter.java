package org.jpc.converter.catalog.jterm;

import org.jconverter.converter.ConversionException;
import org.jpc.Jpc;
import org.jpc.converter.ToTermConverter;
import org.jpc.term.Compound;

public class ToJTermConverter<T> implements ToTermConverter<T, Compound> {

	@Override
	public Compound toTerm(T object, Class<Compound> termClass, Jpc context) {
		Compound compound = context.jTerm(object);
		if(compound == null)
			throw new ConversionException();
		return compound;
	}

}
