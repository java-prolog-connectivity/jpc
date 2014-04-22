package org.jpc.converter.catalog.reflection.reification;

import java.lang.reflect.Type;

import org.jpc.Jpc;
import org.jpc.converter.FromTermConverter;
import org.jpc.term.Compound;
import org.jpc.term.Term;
import org.minitoolbox.reflection.ReflectiveClass;

public class TypeNameFunctorConverter<T> implements FromTermConverter<Compound, T> {

	@Override
	public T fromTerm(Compound term, Type targetType, Jpc jpc) {
		String className = term.getNameString();
		Term sourceTerm = term.arg(1);
		Class<?> targetClass = ReflectiveClass.classForName(className);
		return jpc.fromTerm(sourceTerm, targetClass);
	}

}
