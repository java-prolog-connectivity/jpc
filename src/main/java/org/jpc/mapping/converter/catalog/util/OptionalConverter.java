package org.jpc.mapping.converter.catalog.util;


import static java.util.Arrays.asList;
import static org.jpc.term.TermConstants.EMPTY_OPTIONAL;

import java.lang.reflect.Type;
import java.util.Optional;

import org.jconverter.converter.TypeDomain;
import org.jpc.Jpc;
import org.jpc.mapping.converter.FromTermConverter;
import org.jpc.mapping.converter.ToTermConverter;
import org.jpc.term.Compound;
import org.jpc.term.Term;
import org.typeutils.typewrapper.TypeWrapper;

public class OptionalConverter implements ToTermConverter<Optional<?>, Compound>, FromTermConverter<Compound, Optional<?>> {

    public static final String OPTIONAL_FUNCTOR_NAME = "optional";
    public static final String PRESENT_OPTIONAL_VALUE_WRAPPER = "the";

    @Override
    public Optional<?> fromTerm(Compound term, TypeDomain target, Jpc context) {
        if (term.equals(EMPTY_OPTIONAL)) {
            return Optional.empty();
        } else {
            TypeWrapper wrappedTargetType = TypeWrapper.wrap(target.getType());
            Type componentType = null;
            if (wrappedTargetType.hasActualTypeArguments()) {
                componentType = wrappedTargetType.getActualTypeArguments()[0];
            } else {
                componentType = Object.class;
            }
            Object wrapped = context.fromTerm(term.arg(1).arg(1), componentType);
            return Optional.of(wrapped);
        }
    }

    @Override
    public Compound toTerm(Optional<?> opt, TypeDomain target, Jpc context) {
        if (opt.isPresent()) {
            return new Compound(OPTIONAL_FUNCTOR_NAME, asList(wrapValue(context.toTerm(opt.get()))));
        } else {
            return EMPTY_OPTIONAL;
        }
    }

    private Compound wrapValue(Term term) {
        return new Compound(PRESENT_OPTIONAL_VALUE_WRAPPER, asList(term));
    }

}
