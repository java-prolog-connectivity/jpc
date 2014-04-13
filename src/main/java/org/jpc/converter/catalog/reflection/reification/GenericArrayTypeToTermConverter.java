package org.jpc.converter.catalog.reflection.reification;

import java.lang.reflect.GenericArrayType;

import org.jpc.Jpc;
import org.jpc.converter.ToTermConverter;
import org.jpc.term.Compound;
import org.minitoolbox.reflection.typewrapper.ArrayTypeWrapper;
import org.minitoolbox.reflection.typewrapper.TypeWrapper;

public class GenericArrayTypeToTermConverter implements ToTermConverter<GenericArrayType, Compound> {
	
	@Override
	public Compound toTerm(GenericArrayType type, Class<Compound> termClass, Jpc jpc) {
		return ClassConverter.toTerm((ArrayTypeWrapper) TypeWrapper.wrap(type), jpc);
	}

}