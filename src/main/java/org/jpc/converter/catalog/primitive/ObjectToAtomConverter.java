package org.jpc.converter.catalog.primitive;

import java.lang.reflect.Type;

import org.jpc.Jpc;
import org.jpc.converter.FromTermConverter;
import org.jpc.converter.ToTermConverter;
import org.jpc.term.Atom;

public class ObjectToAtomConverter<T> implements ToTermConverter<T, Atom>, FromTermConverter<Atom, T> {

	@Override
	public T fromTerm(Atom term, Type targetType, Jpc context) {
		return context.convert(term.getName(), targetType);
	}

	@Override
	public Atom toTerm(T object, Class<Atom> termClass, Jpc context) {
		return new Atom(object.toString());
	}

}
