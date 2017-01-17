package org.jpc.converter;


import org.jpc.Jpc;
import org.jpc.term.Term;

public interface SimpleTermConverter<U> extends ToTermConverter<U, Term> {

    @Override
    default Term toTerm(U object, Class<Term> termClass, Jpc context) {
        return toTerm(object, context);
    }

    Term toTerm(U object, Jpc context);

}
