package org.jpc.converter.catalog.reification.type;

import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;

import org.jpc.Jpc;
import org.jpc.converter.FromTermConverter;
import org.jpc.converter.ToTermConverter;
import org.jpc.term.Compound;
import org.jpc.term.Term;
import org.minitoolbox.reflection.reification.ParameterizedTypeImpl;
import org.minitoolbox.reflection.reification.StaticClass;
import org.minitoolbox.reflection.typewrapper.SingleTypeWrapper;
import org.minitoolbox.reflection.typewrapper.TypeWrapper;

public class ParameterizedTypeConverter implements ToTermConverter<ParameterizedType, Compound>, FromTermConverter<Compound, ParameterizedType> {

	@Override
	public Compound toTerm(ParameterizedType type, Class<Compound> termClass, Jpc jpc) {
		return ClassConverter.toTerm((SingleTypeWrapper) TypeWrapper.wrap(type), jpc);
	}

	@Override
	public ParameterizedType fromTerm(Compound term, Type targetType, Jpc jpc) {
		Term staticClassTerm = term.arg(1);
		StaticClass staticClass = jpc.fromTerm(staticClassTerm);
		Type[] actualTypeArguments = jpc.fromTerm(term.arg(2), Type[].class);
		Type ownerType = jpc.fromTerm(term.arg(3), Type.class);
		return new ParameterizedTypeImpl(actualTypeArguments, ownerType, staticClass.getWrappedClass());
	}
	
}
