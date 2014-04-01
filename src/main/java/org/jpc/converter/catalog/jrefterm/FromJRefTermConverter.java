package org.jpc.converter.catalog.jrefterm;

import java.lang.reflect.Type;

import org.jconverter.converter.ConversionException;
import org.jpc.Jpc;
import org.jpc.converter.FromTermConverter;
import org.jpc.term.Compound;

public class FromJRefTermConverter<T> implements FromTermConverter<Compound, T> {

	@Override
	public T fromTerm(Compound term, Type type, Jpc context) {
		T resolved = (T) context.resolveJRefTerm(term);
		if(resolved == null)
			throw new ConversionException();
		return resolved;
	}

}
