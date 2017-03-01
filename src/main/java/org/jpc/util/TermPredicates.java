package org.jpc.util;


import java.util.function.Predicate;

import org.jpc.term.Atom;
import org.jpc.term.Compound;
import org.jpc.term.Functor;
import org.jpc.term.Term;

public abstract class TermPredicates {

    public static Predicate<Term> unifiesWith(Term term) {
        Term compiledTerm = term.compile();
        return compiledTerm::canUnify;
    }

    public static Predicate<Term> hasFunctor(Functor functor) {
        return term -> term.hasFunctor(functor);
    }

    public static Predicate<Term> hasFunctorName(String name) {
        return term -> {
            if (term instanceof Atom) {
                return ((Atom) term).getName().equals(name);
            } else if (term instanceof Compound) {
                    return ((Compound) term).getName().equals(name);
            } else {
                return false;
            }
        };
    }

}
