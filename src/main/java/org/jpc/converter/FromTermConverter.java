package org.jpc.converter;

import static org.jconverter.converter.TypeDomain.typeDomain;

import java.lang.reflect.Type;

import org.jconverter.converter.TypeDomain;
import org.jpc.Jpc;
import org.jpc.term.Term;

public interface FromTermConverter<T extends Term, U> extends JpcConverter {

	default U fromTerm(T term, Type targetType, Jpc context) {
		return fromTerm(term, typeDomain(targetType), context);
	}

	U fromTerm(T term, TypeDomain target, Jpc context);

}
