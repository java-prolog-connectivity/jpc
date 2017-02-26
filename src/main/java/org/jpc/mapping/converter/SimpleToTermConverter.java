package org.jpc.mapping.converter;


import org.jconverter.converter.TypeDomain;
import org.jpc.Jpc;
import org.jpc.term.Term;

public interface SimpleToTermConverter<U> extends ToTermConverter<U, Term> {

    @Override
    default Term toTerm(U object, TypeDomain target, Jpc context) {
        return toTerm(object, context);
    }

    Term toTerm(U object, Jpc context);

}
