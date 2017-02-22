package org.jpc.converter.catalog;

import org.jconverter.converter.TypeDomain;
import org.jpc.Jpc;
import org.jpc.converter.FromTermConverter;
import org.jpc.term.JRef;

public class JRefToObjectConverter<T> implements FromTermConverter<JRef<T>, T> {

	@Override
	public T fromTerm(JRef<T> term, TypeDomain target, Jpc context) {
		return term.getReferent();
	}

}
