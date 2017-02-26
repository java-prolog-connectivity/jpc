package org.jpc.mapping.converter;

import static org.jconverter.converter.TypeDomain.typeDomain;

import org.jconverter.converter.TypeDomain;
import org.jpc.Jpc;
import org.jpc.term.Term;

public interface ToTermConverter<U, T extends Term> extends JpcConverter {

	default T toTerm(U object, Class<T> targetType, Jpc context) {
		return toTerm(object, typeDomain(targetType), context);
	}

	T toTerm(U object, TypeDomain target, Jpc context);
	
}
