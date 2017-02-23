package org.jpc.converter;


import java.lang.reflect.Type;
import java.util.Objects;

import org.jconverter.converter.TypeDomain;
import org.jpc.term.Term;
import org.typetools.typewrapper.TypeWrapper;

public class TermDomain extends TypeDomain {

    private final Term term;

    private TermDomain(Term term) {
        super(term.getClass());
        this.term = term;
    }

    public static TermDomain termDomain(Term term) {
        return new TermDomain(term);
    }

    public TermDomain refine(Type type) {
        if (!TypeWrapper.wrap(type).isWeakAssignableFrom(getType())) {
            throw new RuntimeException();
        }
        return this;
    }

    @Override
    public boolean contains(Object object) {
        if (object instanceof Term) {
            return termDomain((Term) object).isSubsetOf(this);
        } else {
            return false;
        }
    }

    @Override
    public boolean isSubsetOf(TypeDomain otherTypeDomain) {
        if (!super.isSubsetOf(otherTypeDomain)) {
            return false;
        }
        if (TermDomain.class.isAssignableFrom(otherTypeDomain.getClass())) {
            Term otherTerm = ((TermDomain) otherTypeDomain).term;
            return otherTerm.subsumes(term);
        } else {
            return true;
        }
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        if (!super.equals(o)) return false;
        final TermDomain that = (TermDomain) o;
        return Objects.equals(term, that.term);
    }

    @Override
    public int hashCode() {
        return Objects.hash(super.hashCode(), term);
    }
}
