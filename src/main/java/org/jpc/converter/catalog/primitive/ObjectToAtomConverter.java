package org.jpc.converter.catalog.primitive;

import org.jconverter.converter.TypeDomain;
import org.jpc.Jpc;
import org.jpc.converter.FromTermConverter;
import org.jpc.converter.ToTermConverter;
import org.jpc.term.Atom;

public class ObjectToAtomConverter<T> implements ToTermConverter<T, Atom>, FromTermConverter<Atom, T> {

	@Override
	public T fromTerm(Atom term, TypeDomain target, Jpc context) {
		return context.convert(term.getName(), target);
	}

	@Override
	public Atom toTerm(T object, TypeDomain target, Jpc context) {
		return new Atom(object.toString());
	}

}
