package org.jpc.mapping.converter.catalog;


import static org.jconverter.converter.ConversionGoal.conversionGoal;
import static org.jconverter.converter.TypeDomain.typeDomain;
import static org.jpc.term.TermConstants.JAVA_NULL;

import org.jconverter.converter.ConversionError;
import org.jconverter.converter.DelegateConversionException;
import org.jconverter.converter.TypeDomain;
import org.jpc.Jpc;
import org.jpc.mapping.converter.FromTermConverter;
import org.jpc.mapping.converter.ToTermConverter;
import org.jpc.term.Term;

public class NullConverter implements ToTermConverter<Object, Term>, FromTermConverter<Term, Object> {

    @Override
    public Object fromTerm(Term term, TypeDomain target, Jpc context) {
        if (!term.equals(JAVA_NULL)) {
            throw new DelegateConversionException(conversionGoal(term, target));
        }
        return null;
    }

    @Override
    public Term toTerm(Object object, TypeDomain target, Jpc context) {
        if (object != null) {
            throw new DelegateConversionException(conversionGoal(object, target));
        }
        if (typeDomain(JAVA_NULL.getClass()).isSubsetOf(target)) {
            return JAVA_NULL;
        }
        throw new ConversionError(conversionGoal(object, target));
    }
}
