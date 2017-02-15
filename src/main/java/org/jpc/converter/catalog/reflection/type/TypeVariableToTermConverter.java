package org.jpc.converter.catalog.reflection.type;

import static java.util.Arrays.asList;
import static org.jpc.converter.catalog.reflection.type.ReificationConstants.TYPE_VARIABLE_FUNCTOR_NAME;
import static org.jpc.internal.reflection.ReflectionUtil.typeVariable;
import static org.jpc.term.Var.dontCare;

import java.lang.reflect.GenericDeclaration;
import java.lang.reflect.Type;
import java.lang.reflect.TypeVariable;

import org.jconverter.converter.ConversionException;
import org.jconverter.util.typewrapper.TypeWrapper;
import org.jconverter.util.typewrapper.VariableTypeWrapper;
import org.jpc.Jpc;
import org.jpc.converter.FromTermConverter;
import org.jpc.converter.ToTermConverter;
import org.jpc.term.Atom;
import org.jpc.term.Compound;
import org.jpc.term.Term;


public class TypeVariableToTermConverter implements ToTermConverter<TypeVariable<? extends GenericDeclaration>, Compound>, FromTermConverter<Compound, TypeVariable<? extends GenericDeclaration>> {

	@Override
	public Compound toTerm(TypeVariable<? extends GenericDeclaration> type, Class<Compound> termClass, Jpc jpc) {
		VariableTypeWrapper variableTypeWrapper = (VariableTypeWrapper) TypeWrapper.wrap(type);
		Term genericDeclarationTerm;
		GenericDeclaration genericDeclaration = variableTypeWrapper.getGenericDeclaration(); //a class, method or constructor
		if(genericDeclaration != null)
			genericDeclarationTerm = jpc.toTerm(genericDeclaration); //TODO: Fix. Currently will throw a ConversionException if the generic declaration is not a class.
		else
			genericDeclarationTerm = dontCare();
		
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
		return typeVariable(genericDeclaration, name, upperBounds);
	}
	
}
