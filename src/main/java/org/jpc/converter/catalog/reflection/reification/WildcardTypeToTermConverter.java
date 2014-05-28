package org.jpc.converter.catalog.reflection.reification;

import static java.util.Arrays.asList;
import static org.jpc.converter.catalog.reflection.reification.ReificationConstants.TYPE_VARIABLE_FUNCTOR_NAME;

import java.lang.reflect.Type;
import java.lang.reflect.WildcardType;

import org.jpc.Jpc;
import org.jpc.converter.FromTermConverter;
import org.jpc.converter.ToTermConverter;
import org.jpc.term.Compound;
import org.minitoolbox.reflection.reification.WildcardTypeImpl;
import org.minitoolbox.reflection.typewrapper.TypeWrapper;
import org.minitoolbox.reflection.typewrapper.VariableTypeWrapper;

public class WildcardTypeToTermConverter implements ToTermConverter<WildcardType, Compound>, FromTermConverter<Compound, WildcardType> {

	@Override
	public Compound toTerm(WildcardType type, Class<Compound> termClass, Jpc jpc) {
		VariableTypeWrapper variableTypeWrapper = (VariableTypeWrapper) TypeWrapper.wrap(type);
		Type[] upperBounds = variableTypeWrapper.getUpperBounds();
		Type[] lowerBounds = variableTypeWrapper.getLowerBounds();
		return new Compound(TYPE_VARIABLE_FUNCTOR_NAME, asList(jpc.toTerm(upperBounds), jpc.toTerm(lowerBounds)));
	}

	@Override
	public WildcardType fromTerm(Compound term, Type targetType, Jpc jpc) {
		Type[] upperBounds = jpc.fromTerm(term.arg(1), new Type[]{}.getClass());
		Type[] lowerBounds = jpc.fromTerm(term.arg(2), new Type[]{}.getClass());
		return new WildcardTypeImpl(lowerBounds, upperBounds);
	}
	
}
