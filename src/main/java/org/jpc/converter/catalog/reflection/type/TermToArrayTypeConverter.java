package org.jpc.converter.catalog.reflection.type;

import static org.jpc.internal.reflection.ReflectionUtil.genericArrayType;

import java.lang.reflect.Array;
import java.lang.reflect.Type;

import org.jconverter.converter.TypeDomain;
import org.jpc.Jpc;
import org.jpc.converter.FromTermConverter;
import org.jpc.term.Compound;
import org.jpc.term.Term;

public class TermToArrayTypeConverter<T extends Type> implements FromTermConverter<Compound, T> {

	@Override
	public T fromTerm(Compound term, TypeDomain target, Jpc jpc) {
		Term componentTypeTerm = term.arg(1);
		Type componentType = jpc.fromTerm(componentTypeTerm);
		if(componentType instanceof Class)
			return (T) Array.newInstance((Class<?>) componentType, 0).getClass();
		else
			return (T) genericArrayType(componentType);
	}

}
