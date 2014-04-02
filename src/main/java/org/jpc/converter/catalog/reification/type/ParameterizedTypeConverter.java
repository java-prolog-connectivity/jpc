package org.jpc.converter.catalog.reification.type;

import java.lang.reflect.ParameterizedType;

import org.jpc.Jpc;
import org.jpc.converter.ToTermConverter;
import org.jpc.term.Compound;
import org.minitoolbox.reflection.typewrapper.SingleTypeWrapper;
import org.minitoolbox.reflection.typewrapper.TypeWrapper;

public class ParameterizedTypeConverter implements ToTermConverter<ParameterizedType, Compound> {

	@Override
	public Compound toTerm(ParameterizedType type, Class<Compound> termClass, Jpc jpc) {
		return ClassToTermConverter.toTerm((SingleTypeWrapper) TypeWrapper.wrap(type), jpc);
	}

}
