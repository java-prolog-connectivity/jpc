package org.jpc.converter.catalog.reification.type;

import java.lang.reflect.Array;
import java.lang.reflect.Type;

import org.jpc.Jpc;
import org.jpc.converter.FromTermConverter;
import org.jpc.term.Compound;
import org.jpc.term.Term;
import org.minitoolbox.reflection.reification.GenericArrayTypeImpl;

public class TermToArrayTypeConverter<T extends Type> implements FromTermConverter<Compound, T> {

	@Override
	public T fromTerm(Compound term, Type targetType, Jpc jpc) {
		Term componentTypeTerm = term.arg(1);
		Type componentType = jpc.fromTerm(componentTypeTerm);
		if(componentType instanceof Class)
			return (T) Array.newInstance((Class<?>) componentType, 0).getClass();
		else
			return (T) new GenericArrayTypeImpl(componentType);
	}

}
