package org.jpc.converter.catalog.reflection.type;

import java.lang.reflect.GenericArrayType;

import org.jconverter.util.typewrapper.ArrayTypeWrapper;
import org.jconverter.util.typewrapper.TypeWrapper;
import org.jpc.Jpc;
import org.jpc.converter.ToTermConverter;
import org.jpc.term.Compound;


public class GenericArrayTypeToTermConverter implements ToTermConverter<GenericArrayType, Compound> {
	
	@Override
	public Compound toTerm(GenericArrayType type, Class<Compound> termClass, Jpc jpc) {
		return ClassConverter.toTerm((ArrayTypeWrapper) TypeWrapper.wrap(type), jpc);
	}

}
