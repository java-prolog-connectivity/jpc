package org.jpc.converter.catalog.reflection.type;

import java.lang.reflect.GenericArrayType;

import org.jconverter.converter.TypeDomain;
import org.jpc.Jpc;
import org.jpc.converter.ToTermConverter;
import org.jpc.term.Compound;
import org.typetools.typewrapper.ArrayTypeWrapper;
import org.typetools.typewrapper.TypeWrapper;


public class GenericArrayTypeToTermConverter implements ToTermConverter<GenericArrayType, Compound> {
	
	@Override
	public Compound toTerm(GenericArrayType type, TypeDomain target, Jpc jpc) {
		return ClassConverter.toTerm((ArrayTypeWrapper) TypeWrapper.wrap(type), jpc);
	}

}
