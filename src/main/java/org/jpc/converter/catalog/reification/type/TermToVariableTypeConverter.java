package org.jpc.converter.catalog.reification.type;

import java.lang.reflect.GenericDeclaration;
import java.lang.reflect.Type;

import org.jconverter.converter.ConversionException;
import org.jpc.Jpc;
import org.jpc.converter.FromTermConverter;
import org.jpc.term.Atom;
import org.jpc.term.Compound;
import org.jpc.term.Term;
import org.minitoolbox.reflection.reification.TypeVariableImpl;
import org.minitoolbox.reflection.reification.WildcardTypeImpl;

public class TermToVariableTypeConverter<T extends Type> implements FromTermConverter<Compound, T> {

	@Override
	public T fromTerm(Compound term, Type targetType, Jpc jpc) {
		Term nameTerm = term.arg(1);
		if(!(nameTerm instanceof Atom))
			throw new ConversionException();
		String name = ((Atom)nameTerm).getName();
		
		Type[] upperBounds = jpc.fromTerm(term.arg(3), new Type[]{}.getClass());
		if(!name.equals("?")) {
			GenericDeclaration genericDeclaration = jpc.fromTerm(term.arg(2), GenericDeclaration.class);
			return (T) new TypeVariableImpl(genericDeclaration, name, upperBounds);
		} else {
			Type[] lowerBounds = jpc.fromTerm(term.arg(4), new Type[]{}.getClass());
			return (T) new WildcardTypeImpl(lowerBounds, upperBounds);
		}
	}

}
