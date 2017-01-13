package org.jpc.converter.catalog.reflection.reification;

import static org.jpc.internal.reflection.ReflectionUtil.parameterizedType;

import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;

import org.jconverter.util.typewrapper.SingleTypeWrapper;
import org.jconverter.util.typewrapper.TypeWrapper;
import org.jpc.Jpc;
import org.jpc.converter.FromTermConverter;
import org.jpc.converter.ToTermConverter;
import org.jpc.term.Compound;


public class ParameterizedTypeConverter implements ToTermConverter<ParameterizedType, Compound>, FromTermConverter<Compound, ParameterizedType> {

	@Override
	public Compound toTerm(ParameterizedType type, Class<Compound> termClass, Jpc jpc) {
		return ClassConverter.toTerm((SingleTypeWrapper) TypeWrapper.wrap(type), jpc);
	}

	@Override
	public ParameterizedType fromTerm(Compound term, Type targetType, Jpc jpc) {
		Class<?> rawClass = ClassConverter.getRawClass(term, jpc);
		Type[] actualTypeArguments = jpc.fromTerm(term.arg(3), Type[].class);
		Type ownerType = jpc.fromTerm(term.arg(4), Type.class);
		return parameterizedType(actualTypeArguments, ownerType, rawClass);
	}
	
}
