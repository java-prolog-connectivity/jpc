package org.jpc.converter.catalog;

import org.jconverter.converter.TypeDomain;
import org.jpc.Jpc;
import org.jpc.converter.FromTermConverter;
import org.jpc.term.AbstractVar;

public class VarConverter<T> implements FromTermConverter<AbstractVar, T> {
	
	@Override
	public T fromTerm(AbstractVar term, TypeDomain target, Jpc context) {
		return null;
	}
	
}
