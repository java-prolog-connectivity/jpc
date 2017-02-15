package org.jpc.converter.catalog.reflection;


import static java.util.Arrays.asList;

import java.lang.reflect.Type;

import org.jpc.Jpc;
import org.jpc.converter.FromTermConverter;
import org.jpc.converter.ToTermConverter;
import org.jpc.term.Compound;

public class EnumConverter implements ToTermConverter<Enum<?>, Compound>, FromTermConverter<Compound, Enum<?>> {

    public static final String ENUM_FUNCTOR_NAME = "enum";

    @Override
    public Compound toTerm(Enum<?> object, Class<Compound> termClass, Jpc jpc) {
        return jpc.toCompound(ENUM_FUNCTOR_NAME, asList(object.getClass(), object.name()));
    }

    @Override
    public Enum<?> fromTerm(Compound term, Type targetType, Jpc jpc) {
        Class<? extends Enum<?>> enumClass = jpc.fromTerm(term.arg(1));
        String name = jpc.fromTerm(term.arg(2));
        return Enum.valueOf((Class) enumClass, name);
    }

}
