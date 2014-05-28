package org.jpc.converter.catalog.reflection.reification;

import static java.util.Arrays.asList;
import static org.jpc.converter.catalog.reflection.reification.ReificationConstants.TYPE_VARIABLE_FUNCTOR_NAME;

import java.lang.reflect.GenericDeclaration;
import java.lang.reflect.Type;
import java.lang.reflect.TypeVariable;

import org.jconverter.converter.ConversionException;
import org.jpc.Jpc;
import org.jpc.converter.FromTermConverter;
import org.jpc.converter.ToTermConverter;
import org.jpc.term.Atom;
import org.jpc.term.Compound;
import org.jpc.term.Term;
import org.jpc.term.Var;
import org.minitoolbox.reflection.reification.TypeVariableImpl;
import org.minitoolbox.reflection.typewrapper.TypeWrapper;
import org.minitoolbox.reflection.typewrapper.VariableTypeWrapper;

public class TypeVariableToTermConverter implements ToTermConverter<TypeVariable<? extends GenericDeclaration>, Compound>, FromTermConverter<Compound, TypeVariable<? extends GenericDeclaration>> {

	@Override
	public Compound toTerm(TypeVariable<? extends GenericDeclaration> type, Class<Compound> termClass, Jpc jpc) {
		VariableTypeWrapper variableTypeWrapper = (VariableTypeWrapper) TypeWrapper.wrap(type);
		Term genericDeclarationTerm;
		GenericDeclaration genericDeclaration = variableTypeWrapper.getGenericDeclaration(); //a class, method or constructor
		if(genericDeclaration != null)
			genericDeclarationTerm = jpc.toTerm(genericDeclaration); //TODO: Fix. Currently will throw a ConversionException if the generic declaration is not a class.
		else
			genericDeclarationTerm = Var.ANONYMOUS_VAR;
		
		Type[] upperBounds = variableTypeWrapper.getUpperBounds();
		Type[] lowerBounds = variableTypeWrapper.getLowerBounds();
		
		return new Compound(TYPE_VARIABLE_FUNCTOR_NAME, asList(new Atom(variableTypeWrapper.getName()), genericDeclarationTerm, jpc.toTerm(upperBounds), jpc.toTerm(lowerBounds)));
	}
	
	@Override
	public TypeVariable<? extends GenericDeclaration> fromTerm(Compound term, Type targetType, Jpc jpc) {
		Term nameTerm = term.arg(1);
		if(!(nameTerm instanceof Atom))
			throw new ConversionException();
		String name = ((Atom)nameTerm).getName();
		GenericDeclaration genericDeclaration = jpc.fromTerm(term.arg(2), GenericDeclaration.class);
		Type[] upperBounds = jpc.fromTerm(term.arg(3), new Type[]{}.getClass());
		return new TypeVariableImpl<>(genericDeclaration, name, upperBounds);
	}
	
}
