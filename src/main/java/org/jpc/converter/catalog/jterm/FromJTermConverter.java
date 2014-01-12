package org.jpc.converter.catalog.jterm;

import java.lang.reflect.Type;

import org.jconverter.converter.ConversionException;
import org.jpc.Jpc;
import org.jpc.converter.FromTermConverter;
import org.jpc.term.Compound;

public class FromJTermConverter<T> implements FromTermConverter<Compound, T> {

	@Override
	public T fromTerm(Compound term, Type type, Jpc context) {
		T resolved = (T) context.resolveJTerm(term);
		if(resolved == null)
			throw new ConversionException();
		return resolved;
	}

}
