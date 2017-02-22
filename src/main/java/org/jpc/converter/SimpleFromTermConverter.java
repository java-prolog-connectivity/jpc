package org.jpc.converter;


import org.jconverter.converter.TypeDomain;
import org.jpc.Jpc;
import org.jpc.term.Term;

public interface SimpleFromTermConverter<T> extends FromTermConverter<Term, T> {

    default T fromTerm(Term term, TypeDomain target, Jpc context) {
        return fromTerm(term, context);
    }

    T fromTerm(Term term, Jpc context);

}
