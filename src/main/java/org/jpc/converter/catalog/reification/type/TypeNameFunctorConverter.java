package org.jpc.converter.catalog.reification.type;

import java.lang.reflect.Type;

import org.jpc.Jpc;
import org.jpc.converter.FromTermConverter;
import org.jpc.term.Compound;
import org.jpc.term.Term;
import org.minitoolbox.reflection.reification.StaticClass;

public class TypeNameFunctorConverter<T> implements FromTermConverter<Compound, T> {

	@Override
	public T fromTerm(Compound term, Type targetType, Jpc jpc) {
		String className = term.getNameString();
		Term sourceTerm = term.arg(1);
		Class<?> targetClass = StaticClass.classForName(className);
		return jpc.fromTerm(sourceTerm, targetClass);
	}

}
