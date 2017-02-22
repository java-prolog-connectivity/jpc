package org.jpc.converter.catalog.reflection.type;

import static org.jpc.internal.reflection.ReflectionUtil.parameterizedType;

import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;

import org.jconverter.converter.TypeDomain;
import org.jpc.Jpc;
import org.jpc.converter.FromTermConverter;
import org.jpc.converter.ToTermConverter;
import org.jpc.term.Compound;
import org.typetools.typewrapper.SingleTypeWrapper;
import org.typetools.typewrapper.TypeWrapper;


public class ParameterizedTypeConverter implements ToTermConverter<ParameterizedType, Compound>, FromTermConverter<Compound, ParameterizedType> {

	@Override
	public Compound toTerm(ParameterizedType type, TypeDomain target, Jpc jpc) {
		return ClassConverter.toTerm((SingleTypeWrapper) TypeWrapper.wrap(type), jpc);
	}

	@Override
	public ParameterizedType fromTerm(Compound term, TypeDomain target, Jpc jpc) {
		Class<?> rawClass = ClassConverter.getRawClass(term, jpc);
		Type[] actualTypeArguments = jpc.fromTerm(term.arg(3), Type[].class);
		Type ownerType = jpc.fromTerm(term.arg(4), Type.class);
		return parameterizedType(actualTypeArguments, ownerType, rawClass);
	}
	
}
