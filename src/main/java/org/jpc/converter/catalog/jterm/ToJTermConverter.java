package org.jpc.converter.catalog.jterm;

import org.jconverter.converter.ConversionException;
import org.jpc.Jpc;
import org.jpc.converter.ToTermConverter;
import org.jpc.term.Compound;

public class ToJTermConverter implements ToTermConverter<Object, Compound> {

	@Override
	public Compound toTerm(Object object, Class<Compound> termClass, Jpc context) {
		Compound compound = context.jTerm(object);
		if(compound == null)
			throw new ConversionException();
		return compound;
	}

}
