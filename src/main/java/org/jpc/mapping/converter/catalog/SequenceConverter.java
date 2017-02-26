package org.jpc.mapping.converter.catalog;

import org.jconverter.converter.TypeDomain;
import org.jpc.Jpc;
import org.jpc.mapping.converter.FromTermConverter;
import org.jpc.term.Compound;

public class SequenceConverter<T> implements FromTermConverter<Compound, T> {

	@Override
	public T fromTerm(Compound term, TypeDomain target, Jpc jpc) {
		jpc.fromTerm(term.arg(1)); //may have a side effect
		return jpc.fromTerm(term.arg(2), target);
	}

}
