package org.jpc.mapping.converter.catalog;

import org.jconverter.converter.TypeDomain;
import org.jpc.Jpc;
import org.jpc.mapping.converter.TermConvertable;
import org.jpc.mapping.converter.ToTermConverter;
import org.jpc.term.Term;

public class TermConvertableConverter<T extends Term> implements ToTermConverter<TermConvertable<?>, T> {

	@Override
	public T toTerm(TermConvertable<?> object, TypeDomain target, Jpc context) {
		return (T)object.asTerm();
	}

}
