package org.jpc.converter.catalog.reification.type;

import java.lang.reflect.GenericArrayType;

import org.jpc.Jpc;
import org.jpc.converter.ToTermConverter;
import org.jpc.term.Compound;
import org.minitoolbox.reflection.typewrapper.ArrayTypeWrapper;
import org.minitoolbox.reflection.typewrapper.TypeWrapper;

public class GenericArrayTypeToTermConverter implements ToTermConverter<GenericArrayType, Compound> {
	
	@Override
	public Compound toTerm(GenericArrayType type, Class<Compound> termClass, Jpc jpc) {
		return ClassToTermConverter.toTerm((ArrayTypeWrapper) TypeWrapper.wrap(type), jpc);
	}

}
