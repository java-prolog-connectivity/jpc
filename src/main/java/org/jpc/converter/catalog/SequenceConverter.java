package org.jpc.converter.catalog;

import java.lang.reflect.Type;

import org.jpc.Jpc;
import org.jpc.converter.FromTermConverter;
import org.jpc.term.Compound;

public class SequenceConverter<T> implements FromTermConverter<Compound, T> {

	@Override
	public T fromTerm(Compound term, Type targetType, Jpc jpc) {
		jpc.fromTerm(term.arg(1)); //may have a side effect
		return jpc.fromTerm(term.arg(2), targetType);
	}

}
