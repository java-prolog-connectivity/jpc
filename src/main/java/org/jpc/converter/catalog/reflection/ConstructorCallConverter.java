package org.jpc.converter.catalog.reflection;

import static org.jconverter.converter.ConversionGoal.conversionGoal;

import org.jconverter.converter.DelegateConversionException;
import org.jconverter.converter.TypeDomain;
import org.jpc.Jpc;
import org.jpc.converter.FromTermConverter;
import org.jpc.term.Compound;
import org.jpc.util.PrologSpeakingClass;
import org.jpc.util.reification.ReflectiveClass;

import com.google.gson.internal.Primitives;

public class ConstructorCallConverter<T> implements FromTermConverter<Compound, T> {

	@Override
	public T fromTerm(Compound term, TypeDomain target, Jpc jpc) {
		String className = term.getName();
		Class<? extends T> targetClass;
		try {
			targetClass = ReflectiveClass.classForName(className);
		} catch(RuntimeException e) {
			if(e.getCause() instanceof ClassNotFoundException) {
				throw new DelegateConversionException(conversionGoal(term, target));
			} else {
				throw e;
			}
		}
		if(targetClass.isPrimitive() || Primitives.isWrapperType(targetClass)) {
			if(term.arity() == 1) {
				return jpc.fromTerm(term.arg(1), targetClass);
			} else {
				throw new DelegateConversionException(conversionGoal(term, target));
			}
		} else {
			PrologSpeakingClass<? extends T> prologSpeakingClass = new PrologSpeakingClass<>(targetClass, jpc);
			return prologSpeakingClass.newInstance(term.getArgs());
		}
	}

}
