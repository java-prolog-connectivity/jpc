package org.jpc.converter.catalog.reification.type;

import static java.util.Arrays.asList;
import static org.jpc.converter.catalog.reification.type.ReificationConstants.TYPE_FUNCTOR_NAME;

import java.lang.reflect.GenericDeclaration;
import java.lang.reflect.Type;
import java.lang.reflect.TypeVariable;

import org.jpc.Jpc;
import org.jpc.converter.ToTermConverter;
import org.jpc.term.Atom;
import org.jpc.term.Compound;
import org.jpc.term.Term;
import org.jpc.term.Var;
import org.minitoolbox.reflection.typewrapper.TypeWrapper;
import org.minitoolbox.reflection.typewrapper.VariableTypeWrapper;

public class TypeVariableToTermConverter implements ToTermConverter<TypeVariable<? extends GenericDeclaration>, Compound> {

	@Override
	public Compound toTerm(TypeVariable<? extends GenericDeclaration> type, Class<Compound> termClass, Jpc jpc) {
		return toTerm((VariableTypeWrapper) TypeWrapper.wrap(type), jpc);
	}

	public static Compound toTerm(VariableTypeWrapper variableTypeWrapper, Jpc jpc) {
		Term genericDeclarationTerm;
		GenericDeclaration genericDeclaration = variableTypeWrapper.getGenericDeclaration(); //a class, method or constructor
		if(genericDeclaration != null)
			genericDeclarationTerm = jpc.toTerm(genericDeclaration); //TODO: Fix. Currently will throw a ConversionException if the generic declaration is not a class.
		else
			genericDeclarationTerm = Var.ANONYMOUS_VAR;
		
		Type[] upperBounds = variableTypeWrapper.getUpperBounds();
		Type[] lowerBounds = variableTypeWrapper.getLowerBounds();
		
		return new Compound(TYPE_FUNCTOR_NAME, asList(new Atom(variableTypeWrapper.getName()), genericDeclarationTerm, jpc.toTerm(upperBounds), jpc.toTerm(lowerBounds)));
	}
	
}
