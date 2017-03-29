package org.jpc.mapping.converter.catalog;


import static java.util.Arrays.asList;

import java.lang.reflect.Type;
import java.util.Optional;

import org.jconverter.converter.TypeDomain;
import org.jpc.Jpc;
import org.jpc.mapping.converter.FromTermConverter;
import org.jpc.mapping.converter.ToTermConverter;
import org.jpc.term.Compound;
import org.jpc.term.TermConstants;
import org.typeutils.typewrapper.TypeWrapper;

public class OptionalConverter implements ToTermConverter<Optional<?>, Compound>, FromTermConverter<Compound, Optional<?>> {

    public static final String OPTIONAL_FUNCTOR_NAME = "optional";

    @Override
    public Optional<?> fromTerm(Compound term, TypeDomain target, Jpc context) {
        TypeWrapper wrappedTargetType = TypeWrapper.wrap(target.getType());
        Type componentType = null;
        if (wrappedTargetType.hasActualTypeArguments()) {
            componentType = wrappedTargetType.getActualTypeArguments()[0];
        } else {
            componentType = Object.class;
        }

        Object wrapped = context.fromTerm(term.arg(1), componentType);
        return Optional.ofNullable(wrapped);
    }

    @Override
    public Compound toTerm(Optional<?> opt, TypeDomain target, Jpc context) {
        if (opt.isPresent()) {
            return new Compound(OPTIONAL_FUNCTOR_NAME, asList(context.toTerm(opt.get())));
        } else {
            return TermConstants.EMPTY_OPTIONAL;
        }
    }
}
