package org.jpc.converter.catalog.reification.type;

import java.lang.reflect.WildcardType;

import org.jpc.Jpc;
import org.jpc.converter.ToTermConverter;
import org.jpc.term.Compound;
import org.minitoolbox.reflection.typewrapper.TypeWrapper;
import org.minitoolbox.reflection.typewrapper.VariableTypeWrapper;

public class WildcardTypeToTermConverter implements ToTermConverter<WildcardType, Compound> {

	@Override
	public Compound toTerm(WildcardType type, Class<Compound> termClass, Jpc jpc) {
		return TypeVariableToTermConverter.toTerm((VariableTypeWrapper) TypeWrapper.wrap(type), jpc);
	}

}
