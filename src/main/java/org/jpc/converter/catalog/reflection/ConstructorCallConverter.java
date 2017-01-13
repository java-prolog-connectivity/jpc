package org.jpc.converter.catalog.reflection;

import java.lang.reflect.Type;

import org.jconverter.converter.ConversionException;
import org.jpc.Jpc;
import org.jpc.converter.FromTermConverter;
import org.jpc.term.Compound;
import org.jpc.util.PrologSpeakingClass;
import org.jpc.util.reification.ReflectiveClass;

import com.google.gson.internal.Primitives;

public class ConstructorCallConverter<T> implements FromTermConverter<Compound, T> {

	@Override
	public T fromTerm(Compound term, Type targetType, Jpc jpc) {
		String className = term.getNameString();
		Class<? extends T> targetClass;
		try {
			targetClass = ReflectiveClass.classForName(className);
		} catch(RuntimeException e) {
			if(e.getCause() instanceof ClassNotFoundException)
				throw new ConversionException();
			else
				throw e;
		}
		if(targetClass.isPrimitive() || Primitives.isWrapperType(targetClass)) {
			if(term.arity() == 1) {
				return jpc.fromTerm(term.arg(1), targetClass);
			} else
				throw new ConversionException();
		} else {
			PrologSpeakingClass<? extends T> prologSpeakingClass = new PrologSpeakingClass<>(targetClass, jpc);
			return prologSpeakingClass.newInstance(term.getArgs());
		}
	}

}
