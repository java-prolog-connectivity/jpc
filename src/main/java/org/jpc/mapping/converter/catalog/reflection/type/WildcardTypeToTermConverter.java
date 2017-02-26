package org.jpc.mapping.converter.catalog.reflection.type;

import static java.util.Arrays.asList;
import static org.jpc.mapping.converter.catalog.reflection.type.ReificationConstants.TYPE_VARIABLE_FUNCTOR_NAME;

import java.lang.reflect.Type;
import java.lang.reflect.WildcardType;

import org.jconverter.converter.TypeDomain;
import org.jpc.Jpc;
import org.jpc.mapping.converter.FromTermConverter;
import org.jpc.mapping.converter.ToTermConverter;
import org.jpc.term.Compound;
import org.typeutils.reification.WildcardTypeImpl;
import org.typeutils.typewrapper.TypeWrapper;
import org.typeutils.typewrapper.VariableTypeWrapper;


public class WildcardTypeToTermConverter implements ToTermConverter<WildcardType, Compound>, FromTermConverter<Compound, WildcardType> {

	@Override
	public Compound toTerm(WildcardType type, TypeDomain target, Jpc jpc) {
		VariableTypeWrapper variableTypeWrapper = (VariableTypeWrapper) TypeWrapper.wrap(type);
		Type[] upperBounds = variableTypeWrapper.getUpperBounds();
		Type[] lowerBounds = variableTypeWrapper.getLowerBounds();
		return new Compound(TYPE_VARIABLE_FUNCTOR_NAME, asList(jpc.toTerm(upperBounds), jpc.toTerm(lowerBounds)));
	}

	@Override
	public WildcardType fromTerm(Compound term, TypeDomain target, Jpc jpc) {
		Type[] upperBounds = jpc.fromTerm(term.arg(1), new Type[]{}.getClass());
		Type[] lowerBounds = jpc.fromTerm(term.arg(2), new Type[]{}.getClass());
		return new WildcardTypeImpl(lowerBounds, upperBounds);
	}
	
}
