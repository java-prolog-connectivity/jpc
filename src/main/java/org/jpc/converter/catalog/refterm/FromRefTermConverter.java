package org.jpc.converter.catalog.refterm;

import java.lang.reflect.Type;

import org.jconverter.converter.ConversionException;
import org.jpc.Jpc;
import org.jpc.converter.FromTermConverter;
import org.jpc.term.Compound;

public class FromRefTermConverter<T> implements FromTermConverter<Compound, T> {

	@Override
	public T fromTerm(Compound term, Type type, Jpc context) {
		T resolved = (T) context.resolveRefTerm(term);
		if(resolved == null)
			throw new ConversionException();
		return resolved;
	}

}
