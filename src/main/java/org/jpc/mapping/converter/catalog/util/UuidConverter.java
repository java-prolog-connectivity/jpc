package org.jpc.mapping.converter.catalog.util;


import static java.util.Arrays.asList;
import static org.jpc.term.Atom.atom;

import java.util.UUID;

import org.jconverter.converter.TypeDomain;
import org.jpc.Jpc;
import org.jpc.mapping.converter.FromTermConverter;
import org.jpc.mapping.converter.ToTermConverter;
import org.jpc.term.Atom;
import org.jpc.term.Compound;

public class UuidConverter implements ToTermConverter<UUID, Compound>, FromTermConverter<Compound, UUID> {

    public static final String UUID_FUNCTOR_NAME = "uuid";

    @Override
    public UUID fromTerm(Compound compound, TypeDomain target, Jpc context) {
        return UUID.fromString(((Atom)compound.arg(1)).getName());
    }

    @Override
    public Compound toTerm(UUID uuid, TypeDomain target, Jpc context) {
        return new Compound(UUID_FUNCTOR_NAME, asList(atom(uuid.toString())));
    }

}
