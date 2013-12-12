package org.jpc.converter.catalog.termconvertable;

import org.jpc.Jpc;
import org.jpc.converter.TermConvertable;
import org.jpc.converter.ToTermConverter;
import org.jpc.term.Term;

public class TermConvertableConverter<T extends Term> implements ToTermConverter<TermConvertable<?>, T> {

	@Override
	public T toTerm(TermConvertable<?> object, Class<T> termClass, Jpc context) {
		return (T)object.asTerm();
	}

}
